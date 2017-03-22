package se.cambio.cds.gdl.editor.util;

import jsyntaxpane.DefaultSyntaxKit;
import jsyntaxpane.Lexer;

public class GDLSyntaxKit extends DefaultSyntaxKit {

    public GDLSyntaxKit() {
        super(new GDLLexer());
    }

    GDLSyntaxKit(Lexer lexer) {
        super(lexer);
    }
}
