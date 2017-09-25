import '../css/index.scss';
import * as d3 from 'd3';
import 'd3-selection-multi'
import {digraph} from 'digraph'
import {mapToVis} from 'data'

import vis from 'vis'

window.onload = () => {

  const container = document.getElementsByClassName('container')[0];

  const nodes = new vis.DataSet([]);
  const edges = new vis.DataSet([]);

  const options = {
    width: '100%',
    height: '100%',
    layout: {
      improvedLayout: false
    }
  };

  const network = new vis.Network(container, {nodes, edges}, options);




  function connectForever(backoff){
    const socket = new WebSocket("ws://localhost:8080/graph");

    var error = false;
    socket.onclose = function(e){
      if(!error){
        connectForever(0);
      }
      console.log('close')
    };

    socket.onerror = function(e){
      console.log(e, backoff);
      error = true;
      setTimeout(() => connectForever(backoff + 1), Math.pow(2, backoff) * 100);
    };

    socket.onmessage = function(e){
      const json = JSON.parse(e.data);

      console.log(json);
      const mapped = mapToVis(json);
      network.setData(mapped)
    };
  }

  connectForever(0)
};

