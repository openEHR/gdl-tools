#Expected input parameter: the release zip file (format gdl-editor-{VERSION}-bundle.zip)
#i.e. ./osx-builder.sh ../../../target/gdl-editor-1.2.3-bundle.zip
#More info: http://centerkey.com/mac/java/

FOLDERNAME=$(basename "$1")
FOLDERNAME="${FOLDERNAME%-*}"
VERSION="${FOLDERNAME##*-}"
BIN_NAME="GDLEditor"
INIT_CLASS=se.cambio.cds.gdl.editor.view.InitGDLEditor
unzip $1
mkdir -p package/macosx
cp ../resources/img/$BIN_NAME.icns package/macosx
$JAVA_HOME/bin/javapackager -deploy -native dmg \
-srcFiles $FOLDERNAME -appclass $INIT_CLASS -name $BIN_NAME \
-outdir deploy -outfile $BIN_NAME -v
rm -r package
rm -r $FOLDERNAME
mv deploy/bundles/$BIN_NAME-1.0.dmg ./$BIN_NAME-$VERSION.dmg
rm -r deploy
