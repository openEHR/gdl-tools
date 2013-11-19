/*
 * Created on 26-oct-2006
 *


 */
package se.cambio.cds.view.swing.dialogs;

import se.cambio.openehr.view.util.ScreenUtil;

import javax.swing.*;
import javax.swing.text.html.HTMLEditorKit;
import java.awt.*;
/**
 * @author icorram
 *
 */
public class DialogViewInfo extends JDialog {

    private static final long serialVersionUID = 7045006987399987315L;

    private String _info = null;

    private JScrollPane jScrollPane;

    private JTextPane editorPane;

    private JMenuBar principalMenuBar;

    private JMenu archivoMenu;

    private String _scenarioPath;

    //private SaveLogAction saveLogAction;

    public DialogViewInfo(Window window, String title, String info, String scenarioPath, Dimension dimension) {
        super(window, title);
        init(info, scenarioPath, dimension);
    }

    public DialogViewInfo(Window window, String title, String info, String scenarioPath) {
        super(window, title);
        init(info, scenarioPath, new Dimension(800,600));
    }

    private void init(String info, String scenarioPath, Dimension dimension){
        _info = info;
        _scenarioPath = scenarioPath;
        this.setSize(dimension);
        ScreenUtil.centerComponentOnScreen(this, this.getOwner());
//        if (_scenarioPath!=null){
//            this.setJMenuBar(getPrincipalMenuBar());
//        }
        setAlwaysOnTop(true);
        setResizable(true);
        this.setLayout(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.insets = new Insets(3,3,3,3);
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.BOTH;
        this.add(getJScrollPane(), gbc);
        //Puts cursor at the top
        SwingUtilities.invokeLater(
                new ViewPositionSetter(
                        getJScrollPane().getViewport(),
                        new Point(0,0)));
        setVisible(true);
    }

    /*
    public JMenuBar getPrincipalMenuBar() {
	if (principalMenuBar == null) {
	    principalMenuBar = new JMenuBar();
	    principalMenuBar.add(getArchivoMenu());
	}
	return principalMenuBar;
    }

    
    private JMenu getArchivoMenu() {
	if (archivoMenu == null) {
	    archivoMenu = new JMenu();
	    archivoMenu.setText(LanguageManager.getMessage("File"));
	    archivoMenu.add(getSaveLogAction());
	}
	return archivoMenu;
    }

    public Action getSaveLogAction(){
	if (saveLogAction ==null){
	    saveLogAction = new SaveLogAction(_info, _scenarioPath);
	}
	return saveLogAction;
    }
    */

    /**
     * This method initializes jTextPane	
     *
     * @return javax.swing.JTextPane
     */
    private JEditorPane  getEditorPane () {
        if (editorPane == null) {
            editorPane = new JTextPane();
            editorPane.setPreferredSize(new Dimension(270,110));
            editorPane.setEditable(false);
            if (_info.startsWith("<HTML>")){
                editorPane.setEditorKit(new HTMLEditorKit());
            }
            editorPane.setText(_info);
        }
        return editorPane;
    }


    private JScrollPane getJScrollPane() {
        if (jScrollPane == null) {
            jScrollPane = new JScrollPane();
            jScrollPane.setViewportView(getEditorPane());
        }
        return jScrollPane;
    }

    private class ViewPositionSetter implements Runnable{
        JViewport vp;
        Point p;
        public ViewPositionSetter(JViewport vp,Point p){
            this.vp=vp;
            this.p=p;
        }
        public void run(){
            vp.setViewPosition(p);
        }
    }

}  //  @jve:decl-index=0:visual-constraint="124,21"
