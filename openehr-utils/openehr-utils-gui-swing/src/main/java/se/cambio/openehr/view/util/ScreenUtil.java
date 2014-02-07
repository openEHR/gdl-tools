package se.cambio.openehr.view.util;

/**
 * User: Iago.Corbal
 * Date: 2013-11-18
 * Time: 09:19
 */
import java.awt.*;


public class ScreenUtil {

    private static int getScreenID( Component container ) {
        int scrID = 1;
        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        GraphicsDevice[] gd = ge.getScreenDevices();
        for (int i = 0; i < gd.length; i++) {
            GraphicsConfiguration gc = gd[i].getDefaultConfiguration();
            Rectangle r = gc.getBounds();
            if (r.contains(container.getLocation())) {
                scrID = i+1;
            }
        }
        return scrID;
    }

    public static Dimension getScreenDimension( Component container ) {
        if (container!=null){
            Dimension d = new Dimension(0, 0);
            int scrID = getScreenID(container);
            if (scrID > 0) {
                GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
                DisplayMode mode = ge.getScreenDevices()[scrID - 1].getDisplayMode();
                d.setSize(mode.getWidth(), mode.getHeight());
            }
            return d;
        } else{
            return Toolkit.getDefaultToolkit().getScreenSize();
        }
    }

    public static Point getLocationOnCurrentScreen(final Component c) {
        if (c!=null){
            final Rectangle currentScreenBounds = c.getGraphicsConfiguration().getBounds();
            return new Point(currentScreenBounds.x, currentScreenBounds.y);
        }else{
            return new Point(0,0);
        }
    }

    public static void centerComponentOnScreen(Component component, Component parent){
        Dimension screenSize = getScreenDimension(parent);
        Dimension parentSize = null;
        Point parentPoint = null;
        if (parent!=null){
            parentSize = parent.getSize();
            parentPoint = parent.getLocation();
        }else{
            parentSize = new Dimension(0,0);
            parentPoint = new Point(screenSize.width/2, screenSize.height/2);
        }
        int compWidth = component.getWidth();
        int compHeight = component.getHeight();
        int locx = parentPoint.x + (parentSize.width/2) - (compWidth/2);
        int locy = parentPoint.y + (parentSize.height/2) - (compHeight/2);
        component.setLocation(locx,locy);
    }
}