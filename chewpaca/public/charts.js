function createRadarChart(divId, dataList) {
  // Create chart instance
  var chart = am4core.create(divId, am4charts.RadarChart);
  // chart.scrollbarX = new am4core.Scrollbar();

  chart.data = dataList;
  chart.radius = am4core.percent(100);
  chart.innerRadius = am4core.percent(50);
  chart.tooltip.label.fontSize = 10;

  // Create axes
  var categoryAxis = chart.xAxes.push(new am4charts.CategoryAxis());
  categoryAxis.dataFields.category = "category";
  categoryAxis.renderer.grid.template.location = 0;
  categoryAxis.renderer.minGridDistance = 30;
  categoryAxis.tooltip.disabled = true;
  categoryAxis.renderer.minHeight = 110;
  categoryAxis.renderer.grid.template.disabled = true;
  //categoryAxis.renderer.labels.template.disabled = true;
  let labelTemplate = categoryAxis.renderer.labels.template;
  labelTemplate.radius = am4core.percent(-60);
  labelTemplate.location = 0.5;
  labelTemplate.relativeRotation = 90;
  labelTemplate.fontSize = 9;

  var valueAxis = chart.yAxes.push(new am4charts.ValueAxis());
  valueAxis.renderer.grid.template.disabled = true;
  valueAxis.renderer.labels.template.disabled = true;
  valueAxis.tooltip.disabled = true;
  // valueAxis.logarithmic = true;

  // Create series
  var series = chart.series.push(new am4charts.RadarColumnSeries());
  series.sequencedInterpolation = true;
  series.dataFields.valueY = "value";
  series.dataFields.categoryX = "category";
  series.columns.template.strokeWidth = 0;
  series.tooltipText = "{name}({category}): {valueY}";
  series.columns.template.radarColumn.cornerRadius = 10;
  series.columns.template.radarColumn.innerCornerRadius = 0;

  series.tooltip.pointerOrientation = "vertical";
  series.tooltip.label.fontSize = 10;

  // on hover, make corner radiuses bigger
  let hoverState = series.columns.template.radarColumn.states.create("hover");
  hoverState.properties.cornerRadius = 0;
  hoverState.properties.fillOpacity = 1;

  series.columns.template.adapter.add("fill", function (fill, target) {
    return chart.colors.getIndex(target.dataItem.index);
  });

  // Cursor
  chart.cursor = new am4charts.RadarCursor();
  chart.cursor.innerRadius = am4core.percent(50);
  chart.cursor.lineY.disabled = true;
}

function createBarChart(divId, _title, data, color) {
  var container = am4core.create(divId, am4core.Container);
  container.layout = "grid";
  container.fixedWidthGrid = false;
  container.width = am4core.percent(100);
  container.height = am4core.percent(100);

  var chart = container.createChild(am4charts.XYChart);
  chart.width = am4core.percent(100);
  chart.height = am4core.percent(100);

  chart.data = data;

  //chart.titles.template.fontSize = 10;
  //chart.titles.template.textAlign = "left";
  //chart.titles.template.isMeasured = false;
  //chart.titles.create().text = title;

  chart.padding(20, 5, 2, 5);

  var dateAxis = chart.xAxes.push(new am4charts.CategoryAxis());
  dateAxis.dataFields.category = "month";
  dateAxis.renderer.grid.template.disabled = true;
  dateAxis.renderer.labels.template.disabled = true;
  dateAxis.cursorTooltipEnabled = false;

  var valueAxis = chart.yAxes.push(new am4charts.ValueAxis());
  valueAxis.min = 0;
  valueAxis.renderer.grid.template.disabled = true;
  valueAxis.renderer.baseGrid.disabled = true;
  valueAxis.renderer.labels.template.disabled = true;
  valueAxis.cursorTooltipEnabled = false;

  chart.cursor = new am4charts.XYCursor();
  chart.cursor.lineY.disabled = true;

  var series = chart.series.push(new am4charts.ColumnSeries());
  series.tooltipText = "{month}: {value}";
  series.tooltip.label.fontSize = 12;
  // series.dataFields.dateX = "month";
  series.dataFields.categoryX = "month";
  series.dataFields.valueY = "value";
  series.strokeWidth = 0;
  series.fillOpacity = 0.5;
  series.columns.template.propertyFields.fillOpacity = "opacity";
  // series.columns.template.fill = color;

  // Add label
  var labelBullet = series.bullets.push(new am4charts.LabelBullet());
  labelBullet.label.text = "{valueY}";
  labelBullet.locationY = 0.5;
  labelBullet.label.hideOversized = true;
  labelBullet.fontSize = 9;

  series.columns.template.events.on("sizechanged", function (ev) {
    if (ev.target.dataItem && ev.target.dataItem.bullets) {
      var height = ev.target.pixelHeight;
      ev.target.dataItem.bullets.each(function (id, bullet) {
        if (height > 20) {
          bullet.show();
        } else {
          bullet.hide();
        }
      });
    }
  });

  return chart;
}

// Create series
function createSeriesForStack(chart, field, name) {
  // Set up series
  var series = chart.series.push(new am4charts.ColumnSeries());
  series.name = name;
  series.dataFields.valueY = field;
  series.dataFields.categoryX = "bin";
  series.sequencedInterpolation = true;

  // Make it stacked
  series.stacked = true;

  // Configure columns
  series.columns.template.width = am4core.percent(60);
  series.columns.template.tooltipText =
    "[bold]{name}[/]\n{categoryX}: {valueY}";
  series.tooltip.label.fontSize = 10;

  // Add label
  var labelBullet = series.bullets.push(new am4charts.LabelBullet());
  labelBullet.label.text = "{valueY}";
  labelBullet.locationY = 0.5;
  labelBullet.label.hideOversized = true;
  labelBullet.fontSize = 9;

  return series;
}

function createStackChart(chartId, chartData) {
  var newChart = chartData.map((series) => {
    var seriesObj = {};
    series[1].forEach((e) => {
      var newElem = {};
      newElem[series[0]] = e.value;
      seriesObj[e.bin] = newElem;
    });
    return seriesObj;
  });

  const finalData = [];
  const mergedData = _.merge(...newChart);
  _.forEach(mergedData, (value, key) => {
    finalData.push({
      ...value,
      bin: key,
    });
  });

  var chart = am4core.create(chartId, am4charts.XYChart);
  chart.data = finalData;

  // Create axes
  var categoryAxis = chart.xAxes.push(new am4charts.CategoryAxis());
  categoryAxis.dataFields.category = "bin";
  categoryAxis.renderer.grid.template.location = 0;
  categoryAxis.renderer.labels.template.fontSize = 9;

  var valueAxis = chart.yAxes.push(new am4charts.ValueAxis());
  valueAxis.renderer.inside = true;
  valueAxis.renderer.labels.template.disabled = true;
  valueAxis.min = 0;
  valueAxis.renderer.labels.template.fontSize = 9;

  chartData.forEach((series) => {
    createSeriesForStack(chart, series[0], series[0]);
  });

  // Legend
  // chart.legend = new am4charts.Legend();
}

function createLayeredChart(chartId, chartData) {
  var newChart = chartData.map((series) => {
    var seriesObj = {};
    series[1].forEach((e) => {
      var newElem = {};
      newElem[series[0]] = e.value;
      seriesObj[e.bin] = newElem;
    });
    return seriesObj;
  });

  const finalData = [];
  const mergedData = _.merge(...newChart);
  _.forEach(mergedData, (value, key) => {
    finalData.push({
      ...value,
      bin: key,
    });
  });

  console.log("finalData", finalData);

  var chart = am4core.create(chartId, am4charts.XYChart);
  chart.data = finalData;

  // Create axes
  var categoryAxis = chart.xAxes.push(new am4charts.CategoryAxis());
  categoryAxis.dataFields.category = "bin";
  categoryAxis.renderer.grid.template.location = 0;
  categoryAxis.renderer.labels.template.fontSize = 9;
  categoryAxis.frequency = 1;

  var valueAxis = chart.yAxes.push(new am4charts.ValueAxis());
  valueAxis.renderer.inside = true;
  valueAxis.renderer.labels.template.disabled = true;
  valueAxis.min = 0;
  valueAxis.renderer.labels.template.fontSize = 9;

  chartData.forEach((element, idx) => {
    var series = chart.series.push(new am4charts.ColumnSeries());
    series.dataFields.valueY = element[0];
    series.dataFields.categoryX = "bin";
    series.clustered = false;
    series.tooltip.label.fontSize = 9;
    series.columns.template.width = am4core.percent(80);

    var labelBullet = series.bullets.push(new am4charts.LabelBullet());
    labelBullet.label.text = "{valueY}";
    labelBullet.locationY = 0.1;
    labelBullet.label.hideOversized = true;
    labelBullet.fontSize = 9;
  });
}
