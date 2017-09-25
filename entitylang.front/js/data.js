import _ from 'lodash';

export function mapToVis(raw) {
  const copy = _.cloneDeep(raw);

  const data = {nodes: [], edges: copy.edges};

  copy.nodes.forEach(n => {
    data.nodes.push({entity: n.entity, name: n.name, label: n.entity + '.' + n.name})
  });


  const nodeMap = _.keyBy(data.nodes, 'label');

  for (let i = 0; i < data.nodes.length; i++) {
    const node = data.nodes[i];
    node.id = i;

    let type = raw.types[node.label];
    if (type !== undefined) {
      node.label = node.label + ' :: ' + type
    }
    node.shape = 'box'
  }

  for (let i = 0; i < data.edges.length; i++) {
    const edge = data.edges[i];
    edge.from = nodeMap[edge.n1].id;
    edge.to = nodeMap[edge.n2].id;
    edge.label = '[' + edge.label.map(e => `${e.entity}.${e.name}`).join(", ") + ']';
    edge.arrows = 'to'
  }

  //attach entities
  let id = data.nodes.length;
  const entityNames = _.chain(data.nodes)
    .groupBy(e => e.entity)
    .map((nodes, entity) => {
      const entityNode = {label: entity, id};
      data.nodes.push(entityNode);
      nodes.forEach(node => {
        data.edges.push({from: id, to: node.id, dashes: true})
      });
      id++;
    })
    .value();

  console.log(data);
  return data;
}

function invalidateFileSize(state, id) {

  {
    let entities = id;
    entities = state.File_directory.get(entities);
    state = invalidateDirectorySize(state, entities);
  }

  {
    let entities = id;
    entities = state.File_directory.get(entities);
    entities = state.Directory_Files.get(entities);
    entities = entities.map(state.File_directory.get);
    for (let i = 0; i < entities.length; i++) {
      state = invalidateDirectorySize(entities[i]);
    }
  }

  {
    let entities = id;
    entities = state.Directory_subDirectories.get(entities)
    entities = entities.map(state.Directory_subDirectories.get)
    entities = entities.map(state.Directory_subDirectories.get)
    for (let i = 0; i < entities.length; i++) {
      state = invalidatePppSize(state, entities[i])
    }
  }

  {
    let entities = id;
    entities = state.Directory_parent.get(entities)
    if (entities !== undefined) {
      entities = state.Directory_parent.get(entities);
      if (entities !== undefined) {
        entities = state.Directory_parent.get(entities);
      }
    }
    if (entities !== undefined) {
      state = invalidateSubSubSubSize(entities)
    }
  }
}

function invalidateDirectorySize(state, id) {
  state.Directory_size.remove(id);
  const parent = state.Directory_parent.get(id)
  if (parent !== undefined) {
    invalidateDirectorySize(parent)
  }

}