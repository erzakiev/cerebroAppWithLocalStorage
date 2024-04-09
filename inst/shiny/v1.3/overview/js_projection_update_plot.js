// layout for 2D projections
const overview_projection_layout_2D = {
  uirevision: 'true',
  hovermode: 'closest',
  margin: {
    l: 50,
    r: 50,
    b: 50,
    t: 50,
    pad: 4
  },
  legend: {
    itemsizing: 'constant',
  },
  xaxis: {
    autorange: false,
    mirror: true,
    showline: true,
    zeroline: false,
    showgrid: false,
    range: []
  },
  yaxis: {
    autorange: false,
    mirror: true,
    showline: true,
    zeroline: false,
    showgrid: false,
    range: []
  },
  hoverlabel: {
    font: {
      size: 11
    },
    align: 'left'
  }
};

// layout for 3D projections
const overview_projection_layout_3D = {
  uirevision: 'true',
  hovermode: 'closest',
  margin: {
    l: 50,
    r: 50,
    b: 50,
    t: 50,
    pad: 4
  },
  legend: {
    itemsizing: 'constant',
  },
  scene: {
    aspectmode: "manual",
    aspectratio: {
     x: 1, y: 1, z: 1,
    },
    xaxis: {
      autorange: false,
      mirror: true,
      showline: true,
      zeroline: false,
      showgrid: false
    },
    yaxis: {
      autorange: false,
      mirror: true,
      showline: true,
      zeroline: false,
      showgrid: false
    },
    zaxis: {
      autorange: false,
      mirror: true,
      showline: true,
      zeroline: false,
      showgrid: false
    },
  },
  hoverlabel: {
    font: {
      size: 11
    },
    align: 'left'
  }
};

// structure of input data
const overview_projection_default_params = {
  meta: {
    color_type: '',
    traces: [],
    color_variable: ''
  },
  data: {
    x: [],
    y: [],
    z: [],
    customdata: [],
    color: [],
    size: '',
    opacity: '',
    line: {},
    x_range: [],
    y_range: [],
    z_range: [],
    selectedpoints: [],
    reset_axes: true
  },
  hover: {
    hoverinfo: '',
    text: []
  },
  group_centers: {
    group: [],
    x: [],
    y: [],
    z: []
  }
}

// update 2D projection with continuous coloring
shinyjs.updatePlot2DContinuous = function(params) {
  params = shinyjs.getParams(params, overview_projection_default_params);
  const data = [];
  data.push(
    {
      x: params.data.x,
      y: params.data.y,
      xrange: params.data.xrange,
      yrange: params.data.yrange,
      customdata: params.data.identifier,
      mode: 'markers',
      type: 'scattergl',
      selectedpoints: params.data.selectedpoints,
      marker: {
        size: params.data.point_size,
        opacity: params.data.point_opacity,
        line: params.data.point_line,
        color: params.data.color,
        colorscale: 'YlGnBu',
        reversescale: true,
        colorbar: {
          title: {
            text: params.meta.color_variable
          }
        }
      },
      hoverinfo: params.hover.hoverinfo,
      text: params.hover.text,
      showlegend: false
    }
  );
  
  //console.log('printing params.data.selectedpoints');
  //console.log(params.data.selectedpoints);
  
  
  //console.log('printing data.selectedpoints');
  //console.log(data.selectedpoints);
  
  
  const layout_here = Object.assign(overview_projection_layout_2D);
  //if (params.data.reset_axes) {
  //  layout_here.xaxis['autorange'] = true;
  //  layout_here.yaxis['autorange'] = true;
  //} else {
    layout_here.xaxis['autorange'] = false;
    layout_here.xaxis['range'] = params.data.x_range;
    layout_here.yaxis['autorange'] = false;
    layout_here.yaxis['range'] = params.data.y_range;
  //}
  Plotly.react('overview_projection', data, layout_here);
}

// update 3D projection with continuous coloring
shinyjs.updatePlot3DContinuous = function(params) {
  params = shinyjs.getParams(params, overview_projection_default_params);
  const data = [];
  data.push(
    {
      x: params.data.x,
      y: params.data.y,
      z: params.data.z,
      customdata: params.data.identifier,
      mode: 'markers',
      type: 'scatter3d',
      marker: {
        size: params.data.point_size,
        opacity: params.data.point_opacity,
        line: params.data.point_line,
        color: params.data.color,
        colorscale: 'YlGnBu',
        reversescale: true,
        colorbar: {
          title: {
            text: params.meta.color_variable
          }
        }
      },
      hoverinfo: params.hover.hoverinfo,
      text: params.hover.text,
      showlegend: false
    }
  );
  
  const layout_here_3d = Object.assign(overview_projection_layout_3D);
  //if (params.data.reset_axes) {
  //  layout_here_3d.scene.xaxis['autorange'] = true;
  //  layout_here_3d.scene.yaxis['autorange'] = true;
  //  layout_here_3d.scene.zaxis['autorange'] = true;
  //} else {
    layout_here_3d.scene.xaxis['autorange'] = false;
    layout_here_3d.scene.xaxis['range'] = params.data.x_range;
    layout_here_3d.scene.yaxis['autorange'] = false;
    layout_here_3d.scene.yaxis['range'] = params.data.y_range;
    layout_here_3d.scene.zaxis['autorange'] = false;
    layout_here_3d.scene.zaxis['range'] = params.data.z_range;
  //}
  Plotly.react('overview_projection', data, layout_here_3d);
}

// update 2D projection with categorical coloring
shinyjs.updatePlot2DCategorical = function(params) {
  params = shinyjs.getParams(params, overview_projection_default_params);
  const data = [];
  for (let i = 0; i < params.data.x.length; i++) {
    
  if (params.data.color[i].length == 0) {
    data.push(
      {
        x: params.data.x[i],
        y: params.data.y[i],
        name: params.meta.traces[i],
        customdata: params.data.identifier[i],
        mode: 'markers',
        type: 'scattergl',
        marker: {
          size: params.data.point_size,
          opacity: params.data.point_opacity,
          line: params.data.point_line,
          color: params.data.color[i]
        },
        hoverinfo: params.hover.hoverinfo,
        text: params.hover.text[i],
        hoverlabel: {
          bgcolor: params.data.color[i]
        },
        showlegend: true
      }
    );
  } else {
    data.push(
      {
        x: params.data.x[i],
        y: params.data.y[i],
        name: params.meta.traces[i],
        customdata: params.data.identifier[i],
        mode: 'markers',
        type: 'scattergl',
        marker: {
          size: params.data.point_size,
          opacity: params.data.point_opacity,
          line: params.data.point_line,
          color: params.data.color[i]
        },
        hoverinfo: params.hover.hoverinfo,
        text: params.hover.text[i],
        hoverlabel: {
          bgcolor: params.data.color[i]
        },
        showlegend: true,
        selectedpoints: params.data.selectedpoints[i],
        selected: 
        {
          marker: {
            opacity: 0.99
          }
        },
        unselected: 
        {
          marker: {
            opacity: 0.05
          }
        }
      }
    );
  }
  }

  
  data.push({
      xrange: params.data.xrange,
      yrange: params.data.yrange,
      customdata: params.data.identifier
  })
  
  if (params.group_centers.group.length >= 1) {
    data.push(
      {
        x: params.group_centers.x,
        y: params.group_centers.y,
        text: params.group_centers.group,
        type: 'scattergl',
        mode: 'text',
        name: 'Labels',
        textposition: 'middle center',
        textfont: {
          color: '#000000',
          size: 16
        },
        hoverinfo: 'skip',
        inherit: false
      }
    );
  }
  console.log('printing params.data.selectedpoints');
  console.log(params.data.selectedpoints);
  
  
  console.log('printing data.selectedpoints');
  console.log(data.selectedpoints);
  
  const layout_here = Object.assign(overview_projection_layout_2D);
  //if (params.data.reset_axes) {
  //  layout_here.xaxis['autorange'] = true;
  //  layout_here.yaxis['autorange'] = true;
  //} else {
    layout_here.xaxis['autorange'] = false;
    layout_here.xaxis['range'] = params.data.x_range;
    layout_here.yaxis['autorange'] = false;
    layout_here.yaxis['range'] = params.data.y_range;
  //}
  Plotly.react('overview_projection', data, layout_here);
}

// update 3D projection with categorical coloring
shinyjs.updatePlot3DCategorical = function(params) {
  params = shinyjs.getParams(params, overview_projection_default_params);
  const data = [];
  for (let i = 0; i < params.data.x.length; i++) {
    data.push(
      {
        x: params.data.x[i],
        y: params.data.y[i],
        z: params.data.z[i],
        name: params.meta.traces[i],
        mode: 'markers',
        type: 'scatter3d',
        marker: {
          size: params.data.point_size,
          opacity: params.data.point_opacity,
          line: params.data.point_line,
          color: params.data.color[i]
        },
        hoverinfo: params.hover.hoverinfo,
        text: params.hover.text[i],
        hoverlabel: {
          bgcolor: params.data.color[i]
        },
        showlegend: true
      }
    );
  }
  
  if (params.group_centers.group.length >= 1) {
    data.push(
      {
        x: params.group_centers.x,
        y: params.group_centers.y,
        z: params.group_centers.z,
        text: params.group_centers.group,
        type: 'scatter3d',
        mode: 'text',
        name: 'Labels',
        textposition: 'middle center',
        textfont: {
          color: '#000000',
          size: 16
        },
        hoverinfo: 'skip',
        inherit: false
      }
    );
  }
  
  
  const layout_here_3d = Object.assign(overview_projection_layout_3D);
  //if (params.data.reset_axes) {
  //  console.log('params.data.reset_axes was true!!!!!!!!')
  //  layout_here_3d.scene.xaxis['autorange'] = true;
  //  layout_here_3d.scene.yaxis['autorange'] = true;
  //  layout_here_3d.scene.zaxis['autorange'] = true;
  //} else {
    console.log('params.data.reset_axes wasn true!!!!!!!!')
    layout_here_3d.scene.xaxis['autorange'] = false;
    layout_here_3d.scene.xaxis['range'] = params.data.x_range;
    layout_here_3d.scene.yaxis['autorange'] = false;
    layout_here_3d.scene.yaxis['range'] = params.data.y_range;
    layout_here_3d.scene.zaxis['autorange'] = false;
    layout_here_3d.scene.zaxis['range'] = params.data.z_range;
  //}
  Plotly.react('overview_projection', data, layout_here_3d);
}
