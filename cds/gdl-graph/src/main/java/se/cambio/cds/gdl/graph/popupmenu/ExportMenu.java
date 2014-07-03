package se.cambio.cds.gdl.graph.popupmenu;

import com.mxgraph.swing.mxGraphComponent;
import se.cambio.cds.gdl.graph.GDLGraphUtil;
import se.cambio.openehr.util.ExceptionHandler;

import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;


/**
 * @author icorram
 *
 */
public class ExportMenu extends JPopupMenu {

    private static final long serialVersionUID = -5734461135480001487L;

    public ExportMenu(MouseEvent me, mxGraphComponent mxGraphComponent){
        JPopupMenu menu = new JPopupMenu();
        JMenuItem enableInstance =
                new JMenuItem(
                        "Export (SVG)", //TODO i18n
                        null);
        enableInstance.addActionListener(new ExportToSVG(mxGraphComponent));
        menu.add(enableInstance);

        Point pt = SwingUtilities.convertPoint(me.getComponent(), me.getPoint(), mxGraphComponent);
        menu.show(mxGraphComponent, pt.x, pt.y);

    }

    private class ExportToSVG implements ActionListener{
        private mxGraphComponent _mxGraphComponent = null;
        public ExportToSVG(mxGraphComponent mxGraphComponent){
            _mxGraphComponent = mxGraphComponent;
        }
        @Override
        public void actionPerformed(ActionEvent e) {
            JFileChooser fileChooser = new JFileChooser();
            FileNameExtensionFilter filter = new FileNameExtensionFilter(
                    "SVG",
                    new String[] { "svg" });
            //fileChooser.setDialogTitle("");
            fileChooser.setFileFilter(filter);
            File file = new File(fileChooser.getFileSystemView()
                    .getDefaultDirectory() + "/guide-graph.svg");  //TODO Proper name
            fileChooser.setSelectedFile(file);
            int result = fileChooser.showSaveDialog(_mxGraphComponent);
            File svgFile = fileChooser.getSelectedFile();
            if (result != JFileChooser.CANCEL_OPTION) {
                try{
                    FileWriter fileWriter = new FileWriter(svgFile);
                    GDLGraphUtil.exportGraph(fileWriter, _mxGraphComponent.getGraph());
                }catch (IOException e1){
                    ExceptionHandler.handle(e1);
                }

            }
        }
    }
}
