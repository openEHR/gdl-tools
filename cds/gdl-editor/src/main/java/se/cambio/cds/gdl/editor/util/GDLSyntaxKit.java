package se.cambio.cds.gdl.editor.util;

import jsyntaxpane.DefaultSyntaxKit;
import jsyntaxpane.Lexer;

/**
 * User: Iago.Corbal
 * Date: 2013-08-14
 * Time: 09:42
 */
public class GDLSyntaxKit extends DefaultSyntaxKit{

    public GDLSyntaxKit() {
        super(new GDLLexer());
    }

    GDLSyntaxKit(Lexer lexer) {
        super(lexer);
    }
}
