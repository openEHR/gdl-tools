#Expected input parameter: the release zip file (format gdl-editor-{VERSION}-bundle.zip)
#i.e. ./osx-builder.sh ../../../target/gdl-editor-1.2.3-bundle.zip

FOLDERNAME=$(basename "$1")
FOLDERNAME="${FOLDERNAME%-*}"
VERSION="${FOLDERNAME##*-}"
BIN_NAME="GDLEditor"
unzip $1
mkdir -p package/macosx
cp ../resources/img/GDLEditor.icns package/macosx
$JAVA_HOME/bin/javapackager -deploy -native dmg \
-srcFiles $FOLDERNAME -appclass se.cambio.cds.gdl.editor.view.InitGDLEditor -name $BIN_NAME \
-outdir deploy -outfile $BIN_NAME -v
rm -r package
rm -r $FOLDERNAME
mv deploy/bundles/$BIN_NAME-1.0.dmg ./$BIN_NAME-$VERSION.dmg
rm -r deploy
