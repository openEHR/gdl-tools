package se.cambio.openehr.util;

import java.awt.Window;
import java.util.concurrent.Future;

import javax.swing.SwingUtilities;

import se.cambio.openehr.view.dialogs.InfoDialog;

public class WindowManager {
    public static WindowManager _delegate = null;
    public Window _mainWindow = null;
    private InfoDialog _infoDialog = null;
    private String _description = null;

    private WindowManager(){

    }

    public static void registerMainWindow(Window window){
        getDelegate()._mainWindow = window;
    }

    public static Window getMainWindow(){
        return getDelegate()._mainWindow;
    }


    public static void setBusy(String description){
        getInfoDialog().changeLoadingText(description);
        getInfoDialog().start();
    }

    public static void changeLoadingText(String description){
        getDelegate()._description = description;
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                getInfoDialog().changeLoadingText(getDelegate()._description);
            }
        });
    }

    public void changeBusyText(String description){
        getInfoDialog().changeLoadingText(description);
    }

    public static void setCurrentProgress(String msg, double progress){
        getInfoDialog().setCurrentProgress(msg, progress);
    }

    public static void setCurrentThread(Future<?> currentThread){
        getInfoDialog().setCurrentThread(currentThread);
    }

    private static InfoDialog getInfoDialog(){
        if (getDelegate()._infoDialog==null){
            getDelegate()._infoDialog = new InfoDialog(getMainWindow());
        }
        return getDelegate()._infoDialog;
    }

    public static void setFree(){
        getInfoDialog().stop();
    }

    private static WindowManager getDelegate(){
        if (_delegate==null){
            _delegate = new WindowManager();
        }
        return _delegate;
    }
}
