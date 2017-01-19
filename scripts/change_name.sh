#!/bin/bash
shopt -s nullglob # dont loop when file pattern matches no files

function usage {
  echo "usage: [old_group_id:old_name] [group_id:name]"
}

function f2pkg {
  echo ${1//\//.}
}

function pkg2f {
  echo ${1//.//}
}

function refactor_package { # $1: dir $2 from, $3: to
  for f in $1/$(pkg2f $2)/*.scala
  do
    sed -i "s/package $2/package $3/" $f
  done
  for f in $1/$(pkg2f $2)/*.java
  do
    sed -i "s/package $2;/package $3;/" $f
  done
  mkdir -p "$1/$(pkg2f $3)"
  mv $1/$(pkg2f $2)/* "$1/$(pkg2f $3)"
  find $1 -type d -empty -delete
}

function fix_import { # $1: dir, $2 from, $3: to
  for f in $1/*.java
  do
    sed -i "s/import $2;/import $3;/" $f
  done
  for f in $1/*.scala
  do
    sed -i "s/import $2/import $3/" $f
  done
}

IFS=: read OLD_GROUP_ID OLD_NAME <<< $1
IFS=: read GROUP_ID PROJECT_NAME <<< $2

if [[ -z $OLD_NAME || -z $OLD_GROUP_ID || -z $PROJECT_NAME || -z GROUP_ID ]]; then
  usage
  exit 1
fi

OLD_GROUP_ID_FOLDER=$(pkg2f $OLD_GROUP_ID)
GROUP_ID_FOLDER=$(pkg2f $GROUP_ID)

#rename all folders
mv "$OLD_NAME.eclipse/" "$PROJECT_NAME.eclipse/"
mv "$OLD_NAME.eclipse.feature/" "$PROJECT_NAME.eclipse.feature/"
mv "$OLD_NAME.eclipse.site/" "$PROJECT_NAME.eclipse.site/"
mv "$OLD_NAME.example/" "$PROJECT_NAME.example/"
mv "$OLD_NAME.lang/" "$PROJECT_NAME.lang/"
mv "$OLD_NAME.scala/" "$PROJECT_NAME.scala/"

function basic_rename {
  sed -i "
    s/$OLD_NAME/$PROJECT_NAME/2
    s/$OLD_NAME/$PROJECT_NAME/1
    s/$OLD_GROUP_ID/$GROUP_ID/
  " $1
}

#main pom file
sed -i "s/$OLD_NAME/$PROJECT_NAME/" "pom.xml"

#.eclipse files
sed -i "
    s/$OLD_NAME/$PROJECT_NAME/
    0,/<groupId>.*<\/groupId>/s/<groupId>.*<\/groupId>/<groupId>$GROUP_ID<\/groupId>/
  " "$PROJECT_NAME.eclipse/pom.xml"

basic_rename "$PROJECT_NAME.eclipse/META-INF/MANIFEST.MF"

#.eclipse.feature files

basic_rename "$PROJECT_NAME.eclipse.feature/pom.xml"
basic_rename "$PROJECT_NAME.eclipse.feature/feature.xml"

#.eclipse.site files

basic_rename "$PROJECT_NAME.eclipse.site/pom.xml"
basic_rename "$PROJECT_NAME.eclipse.site/site.xml"

#.example files

basic_rename "$PROJECT_NAME.example/pom.xml"
basic_rename "$PROJECT_NAME.example/metaborg.yaml"

#.lang files

refactor_package "$PROJECT_NAME.lang/src/main/strategies" "$OLD_NAME.lang.strategies" "$PROJECT_NAME.lang.strategies"
fix_import $PROJECT_NAME.lang/src/main/strategies/$PROJECT_NAME/lang/strategies "$OLD_GROUP_ID.$OLD_NAME.EditorServices" "$GROUP_ID.$PROJECT_NAME.EditorServices"

sed -i "
    s/$OLD_NAME/$PROJECT_NAME/
    0,/<groupId>.*<\/groupId>/s/<groupId>.*<\/groupId>/<groupId>$GROUP_ID<\/groupId>/
    0,/<groupId>.*<\/groupId>/!{0,/<groupId>.*<\/groupId>/! {0,/<groupId>.*<\/groupId>/s/<groupId>.*<\/groupId>/<groupId>$GROUP_ID<\/groupId>/}}
   " "$PROJECT_NAME.lang/pom.xml"

mv $PROJECT_NAME.lang/trans/$OLD_NAME.str $PROJECT_NAME.lang/trans/$PROJECT_NAME.str
basic_rename $PROJECT_NAME.lang/trans/$PROJECT_NAME.str

mv $PROJECT_NAME.lang/syntax/$OLD_NAME.sdf3 $PROJECT_NAME.lang/syntax/$PROJECT_NAME.sdf3
basic_rename $PROJECT_NAME.lang/syntax/$PROJECT_NAME.sdf3

basic_rename $PROJECT_NAME.lang/metaborg.yaml
basic_rename $PROJECT_NAME.lang/editor/Syntax.esv
basic_rename $PROJECT_NAME.lang/editor/Main.esv
basic_rename $PROJECT_NAME.lang/trans/pp.str
basic_rename $PROJECT_NAME.lang/trans/outline.str

#.scala files
refactor_package "$PROJECT_NAME.scala/src/main/scala" "$OLD_GROUP_ID.$OLD_NAME" "$GROUP_ID.$PROJECT_NAME"
refactor_package "$PROJECT_NAME.scala/src/test/scala" "$OLD_GROUP_ID.$OLD_NAME" "$GROUP_ID.$PROJECT_NAME"

basic_rename $PROJECT_NAME.scala/pom.xml

sed -i "
  s/<include>$OLD_GROUP_ID:$OLD_NAME.scala:\*<\/include>/<include>$GROUP_ID:$PROJECT_NAME.scala:*<\/include>/
" $PROJECT_NAME.scala/src/assembly/assembly.xml
