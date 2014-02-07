package se.cambio.openehr.util;

import org.apache.log4j.Logger;

import javax.swing.*;
import java.awt.*;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.Future;

public class WindowManager {
    public static WindowManager _delegate = null;
    public Window _mainWindow = null;
    public Map<String, ProgressManager> _progressManagerMap = null;
    private String _description = null;

    public static String DFAULT_PROGRESS_MANAGER_KEY = "default";

    private WindowManager(){

    }

    public static void registerMainWindow(Window window){
        getDelegate()._mainWindow = window;
    }

    public static void registerProgressManager(ProgressManager progressManager){
        getProgressManagerMap().put(DFAULT_PROGRESS_MANAGER_KEY,progressManager);
    }

    public static void registerProgressManager(String progressKey, ProgressManager progressManager){
        getProgressManagerMap().put(progressKey,progressManager);
    }

    public static Window getMainWindow(){
        return getDelegate()._mainWindow;
    }


    public static void setBusy(String description){
        getDefaultProgressManager().changeLoadingText(description);
        getDefaultProgressManager().start();
    }

    public static void changeLoadingText(String description){
        getDelegate()._description = description;
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                getDefaultProgressManager().changeLoadingText(getDelegate()._description);
            }
        });
    }

    public void changeBusyText(String description){
        getDefaultProgressManager().changeLoadingText(description);
    }

    public static void setCurrentProgress(String msg, double progress){
        getDefaultProgressManager().setCurrentProgress(msg, progress);
    }

    public static void setCurrentThread(Future<?> currentThread){
        getDefaultProgressManager().setCurrentThread(currentThread);
    }

    public static void setFree(){
        getDefaultProgressManager().stop();
    }

    public static void setBusy(String progressKey, String description){
        getProgressManager(progressKey).changeLoadingText(description);
        getProgressManager(progressKey).start();
    }

    public static void changeLoadingText(final String progressKey, String description){
        getDelegate()._description = description;
        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                getProgressManager(progressKey).changeLoadingText(getDelegate()._description);
            }
        });
    }

    public void changeBusyText(String progressKey, String description){
        getProgressManager(progressKey).changeLoadingText(description);
    }

    public static void setCurrentProgress(String progressKey, String msg, double progress){
        getProgressManager(progressKey).setCurrentProgress(msg, progress);
    }

    public static void setCurrentThread(String progressKey, Future<?> currentThread){
        getProgressManager(progressKey).setCurrentThread(currentThread);
    }

    public static void setFree(String progressKey){
        getProgressManager(progressKey).stop();
    }

    private static ProgressManager getDefaultProgressManager(){
        return getProgressManager(DFAULT_PROGRESS_MANAGER_KEY);
    }

    private static ProgressManager getProgressManager(String progressKey){
        ProgressManager progressManager = getProgressManagerMap().get(progressKey);
        if (progressManager==null){
            Logger.getLogger(WindowManager.class).warn("Progress Manager not found for '" + progressKey + "' using default.");
            progressManager = new DefaultProgressManager();
            getProgressManagerMap().put(progressKey, progressManager);
        }
        return progressManager;
    }

    public static Map<String, ProgressManager> getProgressManagerMap(){
        if (getDelegate()._progressManagerMap==null){
            getDelegate()._progressManagerMap = Collections.synchronizedMap(new LinkedHashMap<String, ProgressManager>());
        }
        return getDelegate()._progressManagerMap;
    }


    private static WindowManager getDelegate(){
        if (_delegate==null){
            _delegate = new WindowManager();
        }
        return _delegate;
    }
}
