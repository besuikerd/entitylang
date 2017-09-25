
function invalidateDirectory_files(state, id){


  {
    //Directory.size
    let entities = id;



    state = invalidateDirectory_Size(entities)
  }


  {
    //Directory.size
    let entities = id;



    entities = state.Directory_Files.get(entities);



    entities = entities.map(state.File_Directory.get);



    for(let i = 0 ; i < entities.length ; i++){
      state = invalidateDirectory_Size(entities[i])
    }
  }

  return state;
}



function invalidateFile_c(state, id){

  return state;
}



function invalidateFile_size(state, id){


  {
    //Directory.size
    let entities = id;



    entities = state.File_Directory.get(entities);



    entities = state.Directory_Files.get(entities);



    entities = entities.map(state.File_Directory.get);



    for(let i = 0 ; i < entities.length ; i++){
      state = invalidateDirectory_Size(entities[i])
    }
  }


  {
    //Directory.size
    let entities = id;



    entities = state.File_Directory.get(entities);



    state = invalidateDirectory_Size(entities)
  }

  return state;
}



function invalidateDirectory_b(state, id){


  {
    //File.c
    let entities = id;



    entities = state.Directory_Files.get(entities);



    for(let i = 0 ; i < entities.length ; i++){
      state = invalidateFile_C(entities[i])
    }
  }

  return state;
}



function invalidateDirectory_parent(state, id){

  return state;
}



function invalidateDirectory_subDirectories(state, id){


  {
    //Directory.size
    let entities = id;



    state = invalidateDirectory_Size(entities)
  }

  return state;
}



function invalidateDirectory_name(state, id){

  return state;
}



function invalidateFile_name(state, id){

  return state;
}



function invalidateFile_directory(state, id){


  {
    //File.c
    let entities = id;



    state = invalidateFile_C(entities)
  }


  {
    //Directory.size
    let entities = id;



    entities = state.File_Directory.get(entities);



    state = invalidateDirectory_Size(entities)
  }

  return state;
}



function invalidateDirectory_size(state, id){


  {
    //Directory.size
    let entities = id;



    entities = state.Directory_Parent.get(entities);



    if(entities !== null){
      state = invalidateDirectory_Size(entities)
    }

  }

  return state;
}



function invalidateDirectory_a(state, id){


  {
    //File.c
    let entities = id;



    entities = state.Directory_Files.get(entities);



    for(let i = 0 ; i < entities.length ; i++){
      state = invalidateFile_C(entities[i])
    }
  }

  return state;
}
