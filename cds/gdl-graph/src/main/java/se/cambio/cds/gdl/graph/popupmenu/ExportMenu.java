package se.cambio.cds.gdl.graph.popupmenu;

import org.jgraph.JGraph;
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

    public ExportMenu(MouseEvent me, JGraph jGraph){
        JPopupMenu menu = new JPopupMenu();
        JMenuItem enableInstance =
                new JMenuItem(
                        "Export (SVG)", //TODO i18n
                        null);
        enableInstance.addActionListener(new ExportToSVG(jGraph));
        menu.add(enableInstance);

        Point pt = SwingUtilities.convertPoint(me.getComponent(), me.getPoint(), jGraph);
        menu.show(jGraph, pt.x, pt.y);

    }

    private class ExportToSVG implements ActionListener{
        private JGraph _jGraph = null;
        public ExportToSVG(JGraph jGraph){
            _jGraph = jGraph;
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
            int result = fileChooser.showSaveDialog(_jGraph);
            File svgFile = fileChooser.getSelectedFile();
            if (result != JFileChooser.CANCEL_OPTION) {
                try{
                    FileWriter fileWriter = new FileWriter(svgFile);
                    GDLGraphUtil.exportGraph(fileWriter, _jGraph);
                }catch (IOException e1){
                    ExceptionHandler.handle(e1);
                }

            }
        }
    }
}
