# C# Demo - Switchback Integration

A data visualization dashboard showcasing **Switchback's instant view switching** paired with **C#'s powerful LINQ query composition** for dynamic data aggregation.

## What's Included

- **Program.cs** - ASP.NET Core Minimal API with LINQ-powered data queries
- **app.ts** - Client-side Switchback app with interactive charts
- **vite.config.ts** - Bundles app.ts + Switchback into single JS file
- **Docker setup** - Multi-stage build with .NET 8 SDK

## What Makes This Special

This demo highlights the synergy between Switchback and C#:

### Switchback's Strength: Instant View Switching
- Switch between Time Series, Comparison, and Distribution views
- Apply filters and change metrics without page reloads
- Smooth navigation with progress indicators
- Single-page app experience with vanilla JavaScript

### C#'s Strength: LINQ Query Composition
- Type-safe data transformations at compile time
- Composable queries that build dynamically based on user input
- Clean, readable aggregation syntax
- Entity Framework Core translates LINQ to optimized queries

## Try It Out

Want to see C# + LINQ in action without installing .NET locally?

```bash
cd examples/demos/csharp
docker-compose up
```

Open http://localhost:8000

Docker handles all the build steps automatically.

## Running Natively

To run this demo with a local .NET installation:

1. Install .NET 8 SDK: https://dotnet.microsoft.com/download
2. Build the frontend:
   ```bash
   cd examples/demos/csharp
   npm install
   npm run build
   ```
3. Run the server:
   ```bash
   dotnet run
   ```
4. Open http://localhost:8000

## The Integration Story

### How Switchback and LINQ Work Together

**User Action**: Click "Show Monthly Temperature"

```
1. Browser:    app.visit('/timeseries')
               ↓
2. Switchback: Intercept navigation, show progress bar
               ↓
3. Browser:    GET /timeseries (with X-Switchback header)
               ↓
4. C#:         Return TimeSeriesView component JSON
               ↓
5. Switchback: Mount component, reset chart state
               ↓
6. Component:  Call fetchChartData('timeseries', {metric: 'temperature', period: 'month'})
               ↓
7. Browser:    GET /api/data/timeseries?metric=temperature&period=month
               ↓
8. C#:         Execute LINQ query:
               query.GroupBy(o => new { o.Date.Year, o.Date.Month })
                    .Select(g => new {
                        label = $"{g.Key.Year}-{g.Key.Month:D2}",
                        value = g.Average(o => o.Temperature)
                    })
               ↓
9. EF Core:    Translate to optimized database query
               ↓
10. C#:        Return JSON: { data: [...] }
               ↓
11. Browser:   Parse JSON, update state
               ↓
12. Switchback: app.reload() triggers re-render
               ↓
13. Browser:   Render bar chart with new data (no page reload!)
```

### LINQ Query Examples

**Time Series Aggregation**
```csharp
// Daily averages
query.GroupBy(o => o.Date.Date)
     .Select(g => new {
         label = g.Key.ToString("MMM dd"),
         value = g.Average(o => o.Temperature)
     })

// Monthly averages
query.GroupBy(o => new { o.Date.Year, o.Date.Month })
     .Select(g => new {
         label = $"{g.Key.Year}-{g.Key.Month:D2}",
         value = g.Average(o => o.Temperature)
     })

// Yearly totals
query.GroupBy(o => o.Date.Year)
     .Select(g => new {
         label = g.Key.ToString(),
         value = g.Sum(o => o.Precipitation)
     })
```

**Dynamic Filtering**
```csharp
var query = db.Observations.AsQueryable();

// Add station filter if provided
if (!string.IsNullOrEmpty(stationFilter))
    query = query.Where(o => o.Station == stationFilter);

// Choose metric dynamically
var value = metric switch {
    "temperature" => g.Average(o => o.Temperature),
    "precipitation" => g.Sum(o => o.Precipitation),
    "humidity" => g.Average(o => o.Humidity),
    _ => g.Average(o => o.Temperature)
};
```

**Station Comparison**
```csharp
query.GroupBy(o => o.Station)
     .Select(g => new {
         station = g.Key,
         avgTemp = g.Average(o => o.Temperature),
         totalPrecip = g.Sum(o => o.Precipitation),
         count = g.Count()
     })
     .OrderBy(x => x.station)
```

## Key Features Demonstrated

- ✅ **Instant view switching** - No page reloads between chart types
- ✅ **Dynamic filtering** - Station, metric, and period selection
- ✅ **LINQ composition** - Queries built based on user input
- ✅ **Type safety** - Compile-time checking of queries and models
- ✅ **EF Core in-memory database** - Fast, zero-config data storage
- ✅ **Minimal API** - Clean, lightweight endpoint definitions
- ✅ **Interactive charts** - Pure CSS bar charts, no external libraries
- ✅ **Progress indicators** - Visual feedback during data loading

## File Structure

```
csharp/
├── Program.cs          # ASP.NET Core Minimal API + LINQ queries
├── app.ts              # Switchback frontend with charts
├── CSharpDemo.csproj   # .NET project file
├── vite.config.ts      # Vite bundler config
├── package.json        # Build scripts
├── Dockerfile          # Docker image
├── docker-compose.yml  # Docker setup
└── README.md           # This file
```

## Why C# + Switchback?

### Advantages of This Stack

| Feature | C# + Switchback | Traditional SPA |
|---------|-----------------|-----------------|
| **Query Composition** | LINQ (type-safe, composable) | String concatenation or ORM helpers |
| **Type Safety** | Compile-time checking | Runtime errors |
| **Bundle Size** | Lightweight Switchback | Heavy React/Vue/Angular |
| **Learning Curve** | Minimal - vanilla JS + C# | Framework-specific patterns |
| **Backend Flexibility** | Any .NET API | Often tied to Node.js |
| **Performance** | Native compiled code | V8 JIT compilation |

### Perfect For

- ✅ **Data-heavy dashboards** - Complex aggregations, multiple views
- ✅ **Enterprise apps** - Type safety, strong tooling, .NET ecosystem
- ✅ **Internal tools** - Quick development, no frontend framework needed
- ✅ **API-first architecture** - Backend can serve multiple clients
- ✅ **Developers who prefer backends** - Minimal JavaScript required

## LINQ vs Other Approaches

### LINQ (C#)
```csharp
var monthly = observations
    .Where(o => o.Station == "Station-A")
    .GroupBy(o => new { o.Date.Year, o.Date.Month })
    .Select(g => new {
        Label = $"{g.Key.Year}-{g.Key.Month:D2}",
        AvgTemp = g.Average(o => o.Temperature),
        Count = g.Count()
    })
    .OrderBy(x => x.Label);
```
✅ Type-safe, composable, readable, optimized by EF Core

### Raw SQL (Many Languages)
```sql
SELECT
    CONCAT(YEAR(date), '-', LPAD(MONTH(date), 2, '0')) AS label,
    AVG(temperature) AS avgTemp,
    COUNT(*) AS count
FROM observations
WHERE station = 'Station-A'
GROUP BY YEAR(date), MONTH(date)
ORDER BY label
```
❌ String concatenation, no type checking, manual parameter handling

### JavaScript (Node.js)
```javascript
const monthly = observations
    .filter(o => o.station === 'Station-A')
    .reduce((acc, o) => {
        const key = `${o.date.getFullYear()}-${o.date.getMonth() + 1}`;
        if (!acc[key]) acc[key] = { temps: [], count: 0 };
        acc[key].temps.push(o.temperature);
        acc[key].count++;
        return acc;
    }, {});
// More code needed to calculate averages and format...
```
❌ Manual aggregation, no database optimization, verbose

## Understanding the Architecture

### Minimal API Pattern

```csharp
// Traditional Controller approach (verbose)
[ApiController]
[Route("api/data")]
public class DataController : ControllerBase
{
    [HttpGet("{viewType}")]
    public IActionResult GetData(string viewType) { ... }
}

// Minimal API approach (concise)
app.MapGet("/api/data/{viewType}", (string viewType, WeatherContext db) => {
    // Handler logic
    return Results.Json(data);
});
```

Benefits:
- Less boilerplate
- Functional style
- Easy to read and maintain
- Perfect for simple APIs

### Entity Framework Core In-Memory

```csharp
// Configuration
builder.Services.AddDbContext<WeatherContext>(options =>
    options.UseInMemoryDatabase("WeatherData"));

// Usage - same as real database!
var avgTemp = db.Observations
    .Where(o => o.Station == "Station-A")
    .Average(o => o.Temperature);
```

Benefits:
- Zero configuration
- Fast for demos and testing
- Swap to SQL Server/PostgreSQL by changing one line
- Full EF Core feature set

## Extending This Demo

### Add More Chart Types

Create a new view in `app.ts`:
```typescript
'ScatterView': (props: any) => Layout(
  h('div', {},
    h('h1', {}, '🔵 Scatter Plot'),
    // Render scatter plot
  )
)
```

Add endpoint in `Program.cs`:
```csharp
app.MapGet("/api/data/scatter", (WeatherContext db) => {
    var data = db.Observations
        .Select(o => new { x = o.Temperature, y = o.Humidity })
        .ToList();
    return Results.Json(new { data });
});
```

### Add Real Database

Replace in-memory database with SQL Server:
```csharp
builder.Services.AddDbContext<WeatherContext>(options =>
    options.UseSqlServer(builder.Configuration.GetConnectionString("Default")));
```

### Add Authentication

Use ASP.NET Core Identity:
```csharp
builder.Services.AddAuthentication()
    .AddJwtBearer();

app.MapGet("/api/data/{viewType}", (string viewType) => { ... })
    .RequireAuthorization();
```

### Add Real-Time Updates

Add SignalR for live data push:
```csharp
builder.Services.AddSignalR();

// Hub
public class WeatherHub : Hub
{
    public async Task BroadcastUpdate(WeatherData data)
    {
        await Clients.All.SendAsync("ReceiveUpdate", data);
    }
}
```

## Performance Considerations

### EF Core Query Optimization

LINQ queries are translated to SQL and executed on the database, not in memory:

```csharp
// GOOD: Filtered on database
var result = db.Observations
    .Where(o => o.Station == "Station-A")  // Executes on DB
    .Average(o => o.Temperature);          // Executes on DB

// BAD: Loads everything into memory first
var result = db.Observations
    .ToList()                              // ❌ Loads all data
    .Where(o => o.Station == "Station-A")  // Filters in memory
    .Average(o => o.Temperature);          // Calculates in memory
```

### Async for I/O Operations

For real databases, use async methods:
```csharp
app.MapGet("/api/data/{viewType}", async (string viewType, WeatherContext db) =>
{
    var data = await query.ToListAsync();  // Non-blocking I/O
    return Results.Json(data);
});
```

## Troubleshooting

**App not loading?**
- Make sure you've run `npm run build` in the demo directory
- Check that `wwwroot/dist/app.js` exists
- Check browser console for errors

**.NET compilation errors?**
- Make sure .NET 8 SDK is installed: `dotnet --version`
- Run `dotnet restore` to restore packages
- Check that Entity Framework Core package is installed

**Charts not rendering?**
- Check browser console for JavaScript errors
- Verify API is returning data: `curl http://localhost:8000/api/data/timeseries`
- Check that `state.chartData` is populated in browser DevTools

**Docker issues?**
- Make sure port 8000 is not in use: `lsof -i :8000` or `netstat -an | grep 8000`
- Rebuild with `docker-compose build --no-cache`
- Check logs: `docker-compose logs`

## Comparison with Other Demos

- **PHP Demo**: Basic navigation with traditional scripting
- **Go Demo**: Concurrent worker pools with goroutines
- **Zig Demo**: Form handling with POST requests
- **Rust Demo**: Embedded SQLite database with type safety
- **C Demo**: Optimistic UI updates
- **C# Demo**: **Data visualization with LINQ query composition**

Each demonstrates different Switchback features with different language strengths. C#'s LINQ makes complex data aggregation elegant and type-safe.

## Learn More

- [LINQ Documentation](https://learn.microsoft.com/en-us/dotnet/csharp/linq/) - Official Microsoft guide
- [Entity Framework Core](https://learn.microsoft.com/en-us/ef/core/) - EF Core documentation
- [ASP.NET Core Minimal APIs](https://learn.microsoft.com/en-us/aspnet/core/fundamentals/minimal-apis) - Minimal API patterns
- [C# Language Features](https://learn.microsoft.com/en-us/dotnet/csharp/) - C# documentation

## Why Not Blazor?

Blazor is great, but this demo shows an alternative approach:

**Blazor** (Full C# stack)
- ✅ Write UI in C#
- ✅ Component model
- ❌ Large WebAssembly bundle (~2MB+)
- ❌ Framework lock-in
- ❌ Requires C# knowledge for frontend

**Switchback + C#** (Hybrid approach)
- ✅ Lightweight vanilla JS frontend
- ✅ C# backend expertise fully utilized
- ✅ Tiny bundle size (~50KB)
- ✅ Use any frontend approach
- ✅ Progressive enhancement friendly

Use Blazor when you want full C# everywhere. Use Switchback when you want a lightweight SPA with a powerful C# API.

## Next Steps

Try modifying the demo to:
1. Add new chart types (line charts, pie charts, scatter plots)
2. Implement date range filtering with LINQ
3. Add sorting and pagination
4. Create a heatmap visualization
5. Add export functionality (CSV, JSON)
6. Implement caching with `IMemoryCache`
7. Add real weather API integration

Have fun exploring data visualization with C#, LINQ, and Switchback!
