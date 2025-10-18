/**
 * C# Weather Dashboard - Switchback + LINQ Integration
 * Demonstrates dynamic data aggregation with C#'s LINQ queries
 * Features instant view switching without page reloads
 */

using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.StaticFiles;
using Microsoft.EntityFrameworkCore;
using System.Text.Json;

var builder = WebApplication.CreateBuilder(args);

// Ensure wwwroot is set as the web root
builder.WebHost.UseWebRoot("wwwroot");

// Configure EF Core with in-memory database
builder.Services.AddDbContext<WeatherContext>(options =>
    options.UseInMemoryDatabase("WeatherData"));

var app = builder.Build();

// Seed database with sample weather data
using (var scope = app.Services.CreateScope())
{
    var db = scope.ServiceProvider.GetRequiredService<WeatherContext>();
    SeedWeatherData(db);
}

// Serve static files (bundled Switchback app)
app.UseStaticFiles();

// Enable routing
app.UseRouting();

// Map endpoints
app.UseEndpoints(endpoints =>
{
    // API: Get aggregated weather data by view type
    endpoints.MapGet("/api/data/{viewType}", (string viewType, WeatherContext db, HttpContext context) =>
{
    var station = context.Request.Query["station"].ToString();
    var metric = context.Request.Query["metric"].ToString() ?? "temperature";
    var period = context.Request.Query["period"].ToString() ?? "month";

    object data = viewType switch
    {
        "timeseries" => GetTimeSeries(db, station, metric, period),
        "comparison" => GetStationComparison(db, metric, period),
        "distribution" => GetDistribution(db, station, metric),
        _ => new { error = "Invalid view type" }
    };

    return Results.Json(data);
    });

    // Page routes
    endpoints.MapGet("/{*path}", (HttpContext context) =>
{
    var path = context.Request.Path.Value ?? "/";
    var isSwitchback = context.Request.Headers["X-Switchback"].ToString() == "true";

    object pageData = path switch
    {
        "/" => new
        {
            component = "Dashboard",
            props = new
            {
                stats = GetDashboardStats(context.RequestServices.GetRequiredService<WeatherContext>()),
                stations = GetStations()
            },
            url = "/"
        },
        "/timeseries" => new
        {
            component = "TimeSeriesView",
            props = new { stations = GetStations() },
            url = "/timeseries"
        },
        "/comparison" => new
        {
            component = "ComparisonView",
            props = new { stations = GetStations() },
            url = "/comparison"
        },
        "/distribution" => new
        {
            component = "DistributionView",
            props = new { stations = GetStations() },
            url = "/distribution"
        },
        "/about" => new
        {
            component = "About",
            props = new
            {
                version = "1.0.0",
                backend = "C# / ASP.NET Core",
                features = new[]
                {
                    "LINQ query composition for data aggregation",
                    "Entity Framework Core in-memory database",
                    "Minimal API architecture",
                    "Type-safe data transformations",
                    "Multiple chart view switching",
                    "Real-time filtering without page reloads"
                }
            },
            url = "/about"
        },
        _ => new
        {
            component = "Error",
            props = new { message = "Page not found" },
            url = path
        }
    };

    if (isSwitchback)
    {
        return Results.Json(pageData);
    }

    var html = $"""
        <!DOCTYPE html>
        <html lang="en">
        <head>
            <meta charset="UTF-8">
            <meta name="viewport" content="width=device-width, initial-scale=1.0">
            <title>C# Weather Dashboard - Switchback</title>
            <script>window.initialPage = {JsonSerializer.Serialize(pageData)};</script>
        </head>
        <body>
            <div data-swbk-app>
                <div style="padding: 2rem; text-align: center; background: #0f172a; color: #3b82f6;">
                    <p>Loading Switchback app...</p>
                </div>
            </div>
            <script type="module" src="/dist/app.js"></script>
        </body>
        </html>
        """;

    return Results.Content(html, "text/html");
    });
});

app.Run("http://0.0.0.0:8000");

// LINQ Query Functions

object GetTimeSeries(WeatherContext db, string? stationFilter, string metric, string period)
{
    var query = db.Observations.AsQueryable();

    if (!string.IsNullOrEmpty(stationFilter))
        query = query.Where(o => o.Station == stationFilter);

    if (period == "day")
    {
        var grouped = query.GroupBy(o => o.Date.Date)
                          .Select(g => new
                          {
                              label = g.Key.ToString("MMM dd"),
                              date = g.Key,
                              value = metric == "temperature" ? g.Average(o => o.Temperature) :
                                     metric == "precipitation" ? g.Sum(o => o.Precipitation) :
                                     metric == "humidity" ? g.Average(o => o.Humidity) :
                                     g.Average(o => o.Temperature)
                          })
                          .OrderBy(x => x.date)
                          .ToList();
        return new { data = grouped };
    }
    else if (period == "month")
    {
        var grouped = query.GroupBy(o => new { o.Date.Year, o.Date.Month })
                          .Select(g => new
                          {
                              label = $"{g.Key.Year}-{g.Key.Month:D2}",
                              value = metric == "temperature" ? g.Average(o => o.Temperature) :
                                     metric == "precipitation" ? g.Sum(o => o.Precipitation) :
                                     metric == "humidity" ? g.Average(o => o.Humidity) :
                                     g.Average(o => o.Temperature)
                          })
                          .OrderBy(x => x.label)
                          .ToList();
        return new { data = grouped };
    }
    else
    {
        var grouped = query.GroupBy(o => o.Date.Year)
                          .Select(g => new
                          {
                              label = g.Key.ToString(),
                              value = metric == "temperature" ? g.Average(o => o.Temperature) :
                                     metric == "precipitation" ? g.Sum(o => o.Precipitation) :
                                     metric == "humidity" ? g.Average(o => o.Humidity) :
                                     g.Average(o => o.Temperature)
                          })
                          .OrderBy(x => x.label)
                          .ToList();
        return new { data = grouped };
    }
}

object GetStationComparison(WeatherContext db, string metric, string period)
{
    var query = db.Observations.AsQueryable();

    var grouped = query.GroupBy(o => o.Station)
                      .Select(g => new
                      {
                          station = g.Key,
                          value = metric == "temperature" ? g.Average(o => o.Temperature) :
                                 metric == "precipitation" ? g.Sum(o => o.Precipitation) :
                                 metric == "humidity" ? g.Average(o => o.Humidity) :
                                 g.Average(o => o.Temperature),
                          count = g.Count()
                      })
                      .OrderBy(x => x.station)
                      .ToList();

    return new { data = grouped };
}

object GetDistribution(WeatherContext db, string? stationFilter, string metric)
{
    var query = db.Observations.AsQueryable();

    if (!string.IsNullOrEmpty(stationFilter))
        query = query.Where(o => o.Station == stationFilter);

    var values = metric switch
    {
        "temperature" => query.Select(o => o.Temperature).ToList(),
        "precipitation" => query.Select(o => o.Precipitation).ToList(),
        "humidity" => query.Select(o => o.Humidity).ToList(),
        _ => query.Select(o => o.Temperature).ToList()
    };

    // Create histogram bins
    var min = values.Min();
    var max = values.Max();
    var binCount = 10;
    var binSize = (max - min) / binCount;

    var bins = Enumerable.Range(0, binCount)
        .Select(i => new
        {
            label = $"{min + i * binSize:F1}",
            value = values.Count(v => v >= min + i * binSize && v < min + (i + 1) * binSize)
        })
        .ToList();

    return new { data = bins };
}

object GetDashboardStats(WeatherContext db)
{
    var observations = db.Observations.ToList();

    return new
    {
        totalObservations = observations.Count,
        stations = observations.Select(o => o.Station).Distinct().Count(),
        avgTemperature = Math.Round(observations.Average(o => o.Temperature), 1),
        totalPrecipitation = Math.Round(observations.Sum(o => o.Precipitation), 1),
        dateRange = new
        {
            start = observations.Min(o => o.Date).ToString("yyyy-MM-dd"),
            end = observations.Max(o => o.Date).ToString("yyyy-MM-dd")
        }
    };
}

string[] GetStations()
{
    return new[] { "Station-A", "Station-B", "Station-C", "Station-D" };
}

void SeedWeatherData(WeatherContext db)
{
    var random = new Random(42);
    var stations = GetStations();
    var startDate = new DateTime(2024, 1, 1);

    for (int day = 0; day < 365; day++)
    {
        foreach (var station in stations)
        {
            var date = startDate.AddDays(day);
            var baseTemp = 15 + 10 * Math.Sin((day / 365.0) * 2 * Math.PI); // Seasonal variation

            db.Observations.Add(new WeatherObservation
            {
                Id = Guid.NewGuid(),
                Station = station,
                Date = date,
                Temperature = baseTemp + random.NextDouble() * 10 - 5,
                Precipitation = random.NextDouble() * 20,
                Humidity = 50 + random.NextDouble() * 40,
                WindSpeed = random.NextDouble() * 30
            });
        }
    }

    db.SaveChanges();
}

// Data Models

public class WeatherObservation
{
    public Guid Id { get; set; }
    public required string Station { get; set; }
    public DateTime Date { get; set; }
    public double Temperature { get; set; }
    public double Precipitation { get; set; }
    public double Humidity { get; set; }
    public double WindSpeed { get; set; }
}

public class WeatherContext : DbContext
{
    public WeatherContext(DbContextOptions<WeatherContext> options) : base(options) { }
    public DbSet<WeatherObservation> Observations { get; set; }
}
