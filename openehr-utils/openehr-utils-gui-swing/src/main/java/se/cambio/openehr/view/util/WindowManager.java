package se.cambio.openehr.view.util;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import se.cambio.openehr.util.DefaultProgressManager;
import se.cambio.openehr.util.ProgressManager;

import javax.swing.*;
import java.awt.*;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.Future;

public class WindowManager {

    private Window mainWindow = null;
    private Map<String, ProgressManager> progressManagerMap = null;
    private String description = null;

    private static String DEFAULT_PROGRESS_MANAGER_KEY = "default";
    private static Logger logger = LoggerFactory.getLogger(WindowManager.class);


    public WindowManager() {

    }

    public void registerMainWindow(Window window) {
        this.mainWindow = window;
    }

    public void registerProgressManager(ProgressManager progressManager) {
        getProgressManagerMap().put(DEFAULT_PROGRESS_MANAGER_KEY, progressManager);
    }

    public void registerProgressManager(String progressKey, ProgressManager progressManager) {
        getProgressManagerMap().put(progressKey, progressManager);
    }

    public Window getMainWindow() {
        return this.mainWindow;
    }


    public void setBusy(final String description) {
        SwingUtilities.invokeLater(() -> {
            getDefaultProgressManager().changeLoadingText(description);
            getDefaultProgressManager().start();
        });
    }

    public void changeLoadingText(String description) {
        this.description = description;
        SwingUtilities.invokeLater(() -> getDefaultProgressManager().changeLoadingText(description));
    }

    public void changeBusyText(String description) {
        getDefaultProgressManager().changeLoadingText(description);
    }

    public void setCurrentProgress(String msg, double progress) {
        getDefaultProgressManager().setCurrentProgress(msg, progress);
    }

    public void setCurrentThread(Future<?> currentThread) {
        getDefaultProgressManager().setCurrentThread(currentThread);
    }

    public void setFree() {
        SwingUtilities.invokeLater(() -> getDefaultProgressManager().stop());
    }

    public void setFreeNow() {
        getDefaultProgressManager().stop();
    }

    public void setBusy(String progressKey, String description) {
        getProgressManager(progressKey).changeLoadingText(description);
        getProgressManager(progressKey).start();
    }

    public void changeLoadingText(final String progressKey, String description) {
        this.description = description;
        SwingUtilities.invokeLater(() -> getProgressManager(progressKey).changeLoadingText(description));
    }

    public void changeLoadingTextNow(final String progressKey, String description) {
        this.description = description;
        getProgressManager(progressKey).changeLoadingText(this.description);
    }

    public void changeBusyText(String progressKey, String description) {
        getProgressManager(progressKey).changeLoadingText(description);
    }

    public void setCurrentProgress(String progressKey, String msg, double progress) {
        getProgressManager(progressKey).setCurrentProgress(msg, progress);
    }

    public void setCurrentThread(String progressKey, Future<?> currentThread) {
        getProgressManager(progressKey).setCurrentThread(currentThread);
    }

    public void setFree(String progressKey) {
        getProgressManager(progressKey).stop();
    }

    private ProgressManager getDefaultProgressManager() {
        return getProgressManager(DEFAULT_PROGRESS_MANAGER_KEY);
    }

    private ProgressManager getProgressManager(String progressKey) {
        ProgressManager progressManager = getProgressManagerMap().get(progressKey);
        if (progressManager == null) {
            logger.warn("Progress Manager not found for '" + progressKey + "' using default.");
            progressManager = new DefaultProgressManager(progressKey);
            getProgressManagerMap().put(progressKey, progressManager);
        }
        return progressManager;
    }

    private Map<String, ProgressManager> getProgressManagerMap() {
        if (progressManagerMap == null) {
            progressManagerMap = Collections.synchronizedMap(new LinkedHashMap<String, ProgressManager>());
        }
        return progressManagerMap;
    }

}
