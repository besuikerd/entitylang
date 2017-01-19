# Spoofax scala template

boilerplate project for Spoofax projects that use scala. To create a project, simply clone this repo and execute the script `scripts/change_name.sh`.

If you want to change the name of the project to com.domain:somelanguage, execute the script with these parameters and the relevant files are updated:

```bash
scripts/change_name org.example:language_name com.domain:somelanguage
```

You have to build the scala module using maven before you build the project. (it places a jar file inside the language project). When you clean the language project, you will have to rebuild the scala project aswell.
