/**
 * Client-side app for C# Weather Dashboard
 * Bundles Switchback directly from source
 * Demonstrates DATA VISUALIZATION with LINQ aggregations
 */

import { newSwitchback } from '../../../src/index.ts';

// Simple JSX-like helper
function h(tag: string, props: any = {}, ...children: any[]) {
  const element = document.createElement(tag);

  Object.keys(props || {}).forEach(key => {
    if (key.startsWith('on')) {
      element.addEventListener(key.slice(2).toLowerCase(), props[key]);
    } else if (key === 'class' || key === 'className') {
      element.className = props[key];
    } else if (key === 'style' && typeof props[key] === 'object') {
      Object.assign(element.style, props[key]);
    } else if (key === 'selected' || key === 'checked' || key === 'disabled') {
      // Boolean attributes - only set if true
      if (props[key]) {
        (element as any)[key] = true;
      }
    } else {
      element.setAttribute(key, props[key]);
    }
  });

  children.flat().forEach(child => {
    if (typeof child === 'string' || typeof child === 'number') {
      element.appendChild(document.createTextNode(String(child)));
    } else if (child instanceof Node) {
      element.appendChild(child);
    } else if (child) {
      element.appendChild(document.createTextNode(String(child)));
    }
  });

  return element;
}

// Inject blue theme CSS
const style = document.createElement('style');
style.textContent = `
  * { margin: 0; padding: 0; box-sizing: border-box; }

  body {
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
    background: #0f172a;
    color: #e2e8f0;
    line-height: 1.6;
  }

  nav {
    background: #1e293b;
    border-bottom: 2px solid #3b82f6;
    padding: 1rem 2rem;
    position: sticky;
    top: 0;
    z-index: 100;
    box-shadow: 0 4px 12px rgba(59, 130, 246, 0.1);
  }

  nav .nav-content {
    max-width: 1400px;
    margin: 0 auto;
    display: flex;
    justify-content: space-between;
    align-items: center;
  }

  nav a {
    color: #60a5fa;
    text-decoration: none;
    margin-right: 1.5rem;
    padding: 0.5rem 1rem;
    border: 1px solid transparent;
    transition: all 0.2s;
    font-weight: 500;
  }

  nav a:hover {
    border-color: #3b82f6;
    background: rgba(59, 130, 246, 0.1);
  }

  .nav-badge {
    background: #3b82f6;
    color: white;
    padding: 0.4rem 0.8rem;
    border-radius: 4px;
    font-size: 0.85rem;
    font-weight: bold;
  }

  main {
    max-width: 1400px;
    margin: 2rem auto;
    padding: 0 2rem;
  }

  .demo-hint {
    background: linear-gradient(135deg, #1e293b 0%, #0f172a 100%);
    border: 2px solid #3b82f6;
    border-radius: 8px;
    padding: 1.5rem;
    margin-bottom: 2rem;
    text-align: center;
  }

  .demo-hint strong {
    color: #3b82f6;
    font-size: 1.1rem;
  }

  h1 {
    font-size: 2.5rem;
    margin-bottom: 1rem;
    color: #3b82f6;
    text-shadow: 0 0 20px rgba(59, 130, 246, 0.3);
  }

  .badge {
    background: linear-gradient(135deg, #3b82f6, #60a5fa);
    color: white;
    padding: 0.3rem 0.8rem;
    border-radius: 6px;
    font-size: 1rem;
    margin-left: 1rem;
    display: inline-block;
  }

  .stats {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
    gap: 1.5rem;
    margin: 2rem 0;
  }

  .stat-card {
    background: linear-gradient(135deg, #1e293b, #0f172a);
    border: 2px solid #334155;
    border-radius: 8px;
    padding: 1.5rem;
    text-align: center;
  }

  .stat-card strong {
    font-size: 2.5rem;
    color: #3b82f6;
    display: block;
    margin-bottom: 0.5rem;
  }

  .controls {
    background: #1e293b;
    border: 2px solid #334155;
    border-radius: 8px;
    padding: 1.5rem;
    margin: 2rem 0;
    display: flex;
    gap: 1rem;
    flex-wrap: wrap;
    align-items: center;
  }

  .controls label {
    color: #94a3b8;
    font-weight: 500;
  }

  .controls select {
    background: #0f172a;
    border: 1px solid #334155;
    color: #e2e8f0;
    padding: 0.5rem 1rem;
    border-radius: 4px;
    font-size: 0.95rem;
  }

  .controls select:focus {
    outline: none;
    border-color: #3b82f6;
  }

  .chart-container {
    background: #1e293b;
    border: 2px solid #334155;
    border-radius: 8px;
    padding: 2rem;
    margin: 2rem 0;
    min-height: 400px;
  }

  .chart-title {
    color: #3b82f6;
    font-size: 1.3rem;
    margin-bottom: 1.5rem;
    font-weight: 600;
  }

  .chart {
    display: flex;
    align-items: flex-end;
    justify-content: space-around;
    height: 300px;
    border-bottom: 2px solid #334155;
    border-left: 2px solid #334155;
    padding: 1rem;
    gap: 0.5rem;
    overflow-x: auto;
    overflow-y: visible;
  }

  .bar {
    flex: 1 0 auto;
    background: linear-gradient(180deg, #3b82f6, #1e40af);
    border-radius: 4px 4px 0 0;
    transition: all 0.3s;
    position: relative;
    min-width: 16px;
    max-width: 80px;
  }

  .bar:hover {
    background: linear-gradient(180deg, #60a5fa, #3b82f6);
    transform: translateY(-4px);
  }

  .bar-label {
    position: absolute;
    bottom: -25px;
    left: 50%;
    transform: translateX(-50%);
    font-size: 0.75rem;
    color: #94a3b8;
    white-space: nowrap;
  }

  .bar-value {
    position: absolute;
    top: -25px;
    left: 50%;
    transform: translateX(-50%);
    font-size: 0.8rem;
    color: #60a5fa;
    font-weight: bold;
  }

  .loading {
    text-align: center;
    padding: 3rem;
    color: #60a5fa;
    font-size: 1.1rem;
  }

  .btn {
    background: linear-gradient(135deg, #3b82f6, #60a5fa);
    color: white;
    border: none;
    padding: 0.75rem 1.5rem;
    border-radius: 6px;
    cursor: pointer;
    font-weight: bold;
    text-decoration: none;
    display: inline-block;
    transition: all 0.2s;
  }

  .btn:hover {
    transform: translateY(-2px);
    box-shadow: 0 4px 12px rgba(59, 130, 246, 0.4);
  }

  ul {
    margin-left: 2rem;
    margin-top: 1rem;
  }

  li {
    margin: 0.5rem 0;
    color: #cbd5e1;
  }
`;
document.head.appendChild(style);

// Chart rendering helper
function renderChart(data: any[], maxValue?: number) {
  if (!data || data.length === 0) {
    return h('div', { class: 'loading' }, 'No data available');
  }

  const max = maxValue || Math.max(...data.map((d: any) => d.value));

  return h('div', { class: 'chart' },
    ...data.map((item: any) => {
      const height = (item.value / max) * 100;
      return h('div', {
        class: 'bar',
        style: { height: `${height}%` }
      },
        h('div', { class: 'bar-value' }, item.value.toFixed(1)),
        h('div', { class: 'bar-label' }, item.label || item.station)
      );
    })
  );
}

// Layout component
function Layout(children: any) {
  const nav = h('nav', {},
    h('div', { class: 'nav-content' },
      h('div', {},
        h('a', { href: '/', 'data-swbk': '' }, '🏠 Home'),
        h('a', { href: '/timeseries', 'data-swbk': '' }, '📈 Time Series'),
        h('a', { href: '/comparison', 'data-swbk': '' }, '📊 Comparison'),
        h('a', { href: '/distribution', 'data-swbk': '' }, '📉 Distribution'),
        h('a', { href: '/about', 'data-swbk': '' }, 'ℹ️  About')
      ),
      h('div', { class: 'nav-badge' }, 'C# + LINQ')
    )
  );

  const main = h('main', {}, children);

  const container = h('div', {});
  container.appendChild(nav);
  container.appendChild(main);
  return container;
}

// Global state for chart data
const state = {
  chartData: [] as any[],
  loading: false,
  hasFetched: false,
  currentView: '', // Track which view we're on
  // Track selected filters for each view
  timeseriesFilters: { station: 'all', metric: 'temperature', period: 'month' },
  comparisonFilters: { metric: 'temperature' },
  distributionFilters: { station: 'all', metric: 'temperature' }
};

// Page Components
const pages: any = {
  'Dashboard': (props: any) => Layout(
    h('div', {},
      h('h1', {}, 'Weather Data Dashboard', h('span', { class: 'badge' }, '☁️ C#')),
      h('div', { class: 'demo-hint' },
        h('strong', {}, '📊 C# + Switchback Demo:'),
        ' Explore weather data with instant view switching powered by LINQ query composition.'
      ),
      h('div', { class: 'stats' },
        h('div', { class: 'stat-card' },
          h('strong', {}, props.stats.totalObservations),
          h('div', {}, 'Total Observations')
        ),
        h('div', { class: 'stat-card' },
          h('strong', {}, props.stats.stations),
          h('div', {}, 'Weather Stations')
        ),
        h('div', { class: 'stat-card' },
          h('strong', {}, props.stats.avgTemperature + '°C'),
          h('div', {}, 'Avg Temperature')
        ),
        h('div', { class: 'stat-card' },
          h('strong', {}, props.stats.totalPrecipitation + 'mm'),
          h('div', {}, 'Total Precipitation')
        )
      ),
      h('p', { style: { marginTop: '2rem', fontSize: '1.1rem', color: '#cbd5e1' } },
        `Data range: ${props.stats.dateRange.start} to ${props.stats.dateRange.end}`
      ),
      h('div', { style: { marginTop: '2rem' } },
        h('a', { href: '/timeseries', 'data-swbk': '', class: 'btn' }, '📈 Explore Data')
      )
    )
  ),

  'TimeSeriesView': (props: any) => {
    // Reset state if switching to a different view
    if (state.currentView !== 'timeseries') {
      state.chartData = [];
      state.loading = false;
      state.hasFetched = false;
      state.currentView = 'timeseries';
    }

    if (!state.hasFetched && !state.loading) {
      const { station, metric, period } = state.timeseriesFilters;
      fetchChartData('timeseries', { station: station !== 'all' ? station : '', metric, period });
    }

    return Layout(
      h('div', {},
        h('h1', {}, '📈 Time Series Analysis'),
        h('div', { class: 'controls' },
          h('label', {}, 'Station:'),
          h('select', {
            onChange: (e: Event) => {
              const station = (e.target as HTMLSelectElement).value;
              state.timeseriesFilters.station = station;
              state.hasFetched = false;
              fetchChartData('timeseries', {
                station: station !== 'all' ? station : '',
                metric: state.timeseriesFilters.metric,
                period: state.timeseriesFilters.period
              });
            }
          },
            h('option', { value: 'all', selected: state.timeseriesFilters.station === 'all' }, 'All Stations'),
            ...props.stations.map((s: string) =>
              h('option', { value: s, selected: state.timeseriesFilters.station === s }, s)
            )
          ),
          h('label', {}, 'Metric:'),
          h('select', {
            name: 'metric',
            onChange: (e: Event) => {
              const metric = (e.target as HTMLSelectElement).value;
              state.timeseriesFilters.metric = metric;
              state.hasFetched = false;
              fetchChartData('timeseries', {
                station: state.timeseriesFilters.station !== 'all' ? state.timeseriesFilters.station : '',
                metric,
                period: state.timeseriesFilters.period
              });
            }
          },
            h('option', { value: 'temperature', selected: state.timeseriesFilters.metric === 'temperature' }, 'Temperature'),
            h('option', { value: 'precipitation', selected: state.timeseriesFilters.metric === 'precipitation' }, 'Precipitation'),
            h('option', { value: 'humidity', selected: state.timeseriesFilters.metric === 'humidity' }, 'Humidity')
          ),
          h('label', {}, 'Period:'),
          h('select', {
            name: 'period',
            onChange: (e: Event) => {
              const period = (e.target as HTMLSelectElement).value;
              state.timeseriesFilters.period = period;
              state.hasFetched = false;
              fetchChartData('timeseries', {
                station: state.timeseriesFilters.station !== 'all' ? state.timeseriesFilters.station : '',
                metric: state.timeseriesFilters.metric,
                period
              });
            }
          },
            h('option', { value: 'day', selected: state.timeseriesFilters.period === 'day' }, 'Daily'),
            h('option', { value: 'month', selected: state.timeseriesFilters.period === 'month' }, 'Monthly'),
            h('option', { value: 'year', selected: state.timeseriesFilters.period === 'year' }, 'Yearly')
          )
        ),
        h('div', { class: 'chart-container' },
          h('div', { class: 'chart-title' }, 'Temperature Over Time'),
          state.loading
            ? h('div', { class: 'loading' }, 'Loading chart data...')
            : renderChart(state.chartData)
        )
      )
    );
  },

  'ComparisonView': (props: any) => {
    // Reset state if switching to a different view
    if (state.currentView !== 'comparison') {
      state.chartData = [];
      state.loading = false;
      state.hasFetched = false;
      state.currentView = 'comparison';
    }

    if (!state.hasFetched && !state.loading) {
      fetchChartData('comparison', { metric: state.comparisonFilters.metric });
    }

    return Layout(
      h('div', {},
        h('h1', {}, '📊 Station Comparison'),
        h('div', { class: 'controls' },
          h('label', {}, 'Metric:'),
          h('select', {
            onChange: (e: Event) => {
              const metric = (e.target as HTMLSelectElement).value;
              state.comparisonFilters.metric = metric;
              state.hasFetched = false;
              fetchChartData('comparison', { metric });
            }
          },
            h('option', { value: 'temperature', selected: state.comparisonFilters.metric === 'temperature' }, 'Avg Temperature'),
            h('option', { value: 'precipitation', selected: state.comparisonFilters.metric === 'precipitation' }, 'Total Precipitation'),
            h('option', { value: 'humidity', selected: state.comparisonFilters.metric === 'humidity' }, 'Avg Humidity')
          )
        ),
        h('div', { class: 'chart-container' },
          h('div', { class: 'chart-title' }, 'Compare Stations'),
          state.loading
            ? h('div', { class: 'loading' }, 'Loading chart data...')
            : renderChart(state.chartData)
        )
      )
    );
  },

  'DistributionView': (props: any) => {
    // Reset state if switching to a different view
    if (state.currentView !== 'distribution') {
      state.chartData = [];
      state.loading = false;
      state.hasFetched = false;
      state.currentView = 'distribution';
    }

    if (!state.hasFetched && !state.loading) {
      const { station, metric } = state.distributionFilters;
      fetchChartData('distribution', { station: station !== 'all' ? station : '', metric });
    }

    return Layout(
      h('div', {},
        h('h1', {}, '📉 Distribution Analysis'),
        h('div', { class: 'controls' },
          h('label', {}, 'Station:'),
          h('select', {
            onChange: (e: Event) => {
              const station = (e.target as HTMLSelectElement).value;
              state.distributionFilters.station = station;
              state.hasFetched = false;
              fetchChartData('distribution', {
                station: station !== 'all' ? station : '',
                metric: state.distributionFilters.metric
              });
            }
          },
            h('option', { value: 'all', selected: state.distributionFilters.station === 'all' }, 'All Stations'),
            ...props.stations.map((s: string) =>
              h('option', { value: s, selected: state.distributionFilters.station === s }, s)
            )
          ),
          h('label', {}, 'Metric:'),
          h('select', {
            name: 'metric',
            onChange: (e: Event) => {
              const metric = (e.target as HTMLSelectElement).value;
              state.distributionFilters.metric = metric;
              state.hasFetched = false;
              fetchChartData('distribution', {
                station: state.distributionFilters.station !== 'all' ? state.distributionFilters.station : '',
                metric
              });
            }
          },
            h('option', { value: 'temperature', selected: state.distributionFilters.metric === 'temperature' }, 'Temperature'),
            h('option', { value: 'precipitation', selected: state.distributionFilters.metric === 'precipitation' }, 'Precipitation'),
            h('option', { value: 'humidity', selected: state.distributionFilters.metric === 'humidity' }, 'Humidity')
          )
        ),
        h('div', { class: 'chart-container' },
          h('div', { class: 'chart-title' }, 'Value Distribution'),
          state.loading
            ? h('div', { class: 'loading' }, 'Loading chart data...')
            : renderChart(state.chartData)
        )
      )
    );
  },

  'About': (props: any) => Layout(
    h('div', {},
      h('h1', {}, 'ℹ️  About This Demo'),
      h('p', { style: { margin: '1.5rem 0', fontSize: '1.1rem' } },
        h('strong', { style: { color: '#3b82f6' } }, '🔧 Backend: '),
        props.backend
      ),
      h('h2', { style: { color: '#3b82f6', marginTop: '2rem', marginBottom: '1rem' } },
        'Key Features:'
      ),
      h('ul', {},
        ...props.features.map((f: string) => h('li', {}, `✓ ${f}`))
      ),
      h('h2', { style: { color: '#3b82f6', marginTop: '2rem', marginBottom: '1rem' } },
        'What Makes This Special:'
      ),
      h('p', { style: { margin: '1rem 0', color: '#cbd5e1' } },
        'This demo showcases how Switchback\'s instant view switching pairs perfectly with ',
        'C#\'s LINQ query composition. Switch between different chart views, filters, and ',
        'aggregations without any page reloads - all powered by type-safe LINQ queries on the backend.'
      ),
      h('a', { href: '/', 'data-swbk': '', class: 'btn', style: { marginTop: '2rem' } }, '← Back to Dashboard')
    )
  ),

  'Error': (props: any) => Layout(
    h('div', {},
      h('h1', {}, '❌ Error'),
      h('p', {}, props.message),
      h('a', { href: '/', 'data-swbk': '', class: 'btn', style: { marginTop: '2rem' } }, '← Back Home')
    )
  )
};

// Fetch chart data from API
async function fetchChartData(viewType: string, params: any = {}) {
  state.loading = true;
  state.hasFetched = true;

  const query = new URLSearchParams(params).toString();
  const response = await fetch(`/api/data/${viewType}?${query}`);
  const result = await response.json();

  state.chartData = result.data || [];
  state.loading = false;
  app.reload(); // Re-render with chart data
}

// Initialize Switchback
const app = newSwitchback({
  resolve: (name: string) => {
    const component = pages[name];
    if (!component) {
      throw new Error(`Component "${name}" not found`);
    }
    return component;
  },

  setup: ({ el, App, props }: any) => {
    el.innerHTML = '';
    el.appendChild(App(props));
  },

  initialPage: (window as any).initialPage,

  progress: {
    delay: 250,
    color: '#3b82f6',
    includeCSS: true,
    showSpinner: true,
  },
});

console.log('📊 C# Weather Dashboard initialized with LINQ-powered data aggregation!');
