require('d3-selection-multi');
import _ from 'lodash';


function arrowhead(e){
  e
    .append('defs')
    .append('marker')
    .attrs({
      id: 'arrow',
      viewBox: '0 -5 10 10',
      refX: 5,
      refY: 0,
      orient: 'auto'
    })
    .append('path')
    .attr('d', 'M0,-5L10,0L0,5')
    .attr('class', 'arrowhead')
}


export function digraph(graph){
  graph = _.cloneDeep(graph);

  const nodeLookup = _.keyBy(graph.nodes, 'label');

  return (container) => {
    const width = 800;
    const height = 600;

    const size = 25;

    const simulation = d3.forceSimulation()
      .force('charge' , d3.forceManyBody())
      // .force('collide', d3.forceCollide(size))
      .force('center', d3.forceCenter(width / 2, height / 2));

    const svg = d3.select(container)
      .append('svg')
      .attr('width', width)
      .attr('height', height)
      .call(arrowhead)


    const edges = svg.append('g')
      .attr('class', 'links')
      .selectAll('.edge')
      .data(graph.edges)
      .enter()
      .append('line')
      .attr('marker-end', 'url(#arrow)')
      .attr('stroke', 'black')
      .attr('stroke-width', 3)
      .attr('class', 'edge');


    const nodes = svg.append('g')
      .attr('class', 'nodes')
      .selectAll('.node')
      .data(graph.nodes)
      .enter()
      .append('g')
      .attr('class', 'node');

    nodes.append('rect')
      .attr('width', 100)
      .attr('height', 50)
      .attr('fill', 'transparent')
      .attr('r', size);

    nodes.append('text').text(d => d.label)

    simulation
      .nodes(graph.nodes)
      .on('tick', () => {
        edges
          .attr('x1', (d) => nodeLookup[d.from].x)
          .attr('y1', (d) => nodeLookup[d.from].y)
          .attr('x2', (d) => nodeLookup[d.to].x)
          .attr('y2', (d) => nodeLookup[d.to].y);

        nodes
          .attr('transform', (d) => `translate(${d.x}, ${d.y})`)
      });
  }
}