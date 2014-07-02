/*
 * Created on 13-dic-2006
 *


 */
package se.cambio.cds.gdl.graph;

import org.jgraph.JGraph;
import se.cambio.cds.gdl.graph.popupmenu.ExportMenu;
import se.cambio.openehr.util.ExceptionHandler;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseEvent;


/**
 * @author icorram
 *
 *
 */
public class GDLGraphContextMenu extends EventQueue{
    protected void dispatchEvent(AWTEvent event){
        try{
            super.dispatchEvent(event);
        }catch(Exception e){
            ExceptionHandler.handle(e);
        }

        // interested only in mouseevents
        if(!(event instanceof MouseEvent)){
            return;
        }

        MouseEvent me = (MouseEvent)event;

        // interested only in popuptriggers
        if(!me.isPopupTrigger()){
            return;
        }

        // me.getComponent(...) retunrs the heavy weight component on which event occured
        Component comp = SwingUtilities.getDeepestComponentAt(me.getComponent(), me.getX(), me.getY());


        // no popup shown by user code
        if(MenuSelectionManager.defaultManager().getSelectedPath().length>0){
            return;
        }

        // Selection tree
        if(comp instanceof JGraph){
            new ExportMenu(me, (JGraph)comp);
        }
    }
} 