package se.cambio.openehr.util;

import org.apache.log4j.Logger;

import javax.swing.*;
import java.awt.*;
import java.util.concurrent.Future;

public class WindowManager {
    public static WindowManager _delegate = null;
    public Window _mainWindow = null;
    public ProgressManager _progressManager = null;
    private String _description = null;

    private WindowManager(){

    }

    public static void registerMainWindow(Window window){
        getDelegate()._mainWindow = window;
    }

    public static void registerProgressManage(ProgressManager progressManager){
        getDelegate()._progressManager = progressManager;
    }

    public static Window getMainWindow(){
        return getDelegate()._mainWindow;
    }


    public static void setBusy(String description){
        getDelegate()._progressManager.changeLoadingText(description);
        getDelegate()._progressManager.start();
    }

    public static void changeLoadingText(String description){
        getDelegate()._description = description;
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                getDelegate()._progressManager.changeLoadingText(getDelegate()._description);
            }
        });
    }

    public void changeBusyText(String description){
        getDelegate()._progressManager.changeLoadingText(description);
    }

    public static void setCurrentProgress(String msg, double progress){
        getDelegate()._progressManager.setCurrentProgress(msg, progress);
    }

    public static void setCurrentThread(Future<?> currentThread){
        getDelegate()._progressManager.setCurrentThread(currentThread);
    }

    public static void setFree(){
        getDelegate()._progressManager.stop();
    }


    private static WindowManager getDelegate(){
        if (_delegate==null){
            _delegate = new WindowManager();
        }
        return _delegate;
    }
}
