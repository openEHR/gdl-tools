package se.cambio.openehr.view.panels;

import lombok.extern.slf4j.Slf4j;
import org.slf4j.LoggerFactory;
import se.cambio.openehr.util.OpenEHRImageUtil;
import se.cambio.openehr.util.OpenEHRLanguageManager;
import se.cambio.openehr.util.ProgressManager;

import javax.swing.*;
import java.awt.*;
import java.util.concurrent.Future;

@Slf4j
public class ProgressBarPanel extends JPanel implements ProgressManager {

    private JProgressBar jProgressBar = null;
    private Future<?> _currentThread = null;
    private boolean isActive = false;
    private boolean showCancelButton = true;
    private int _progressValue = 0;
    private String _description = null;
    private JButton cancelButton;
    private JLabel descriptionLabel = null;
    private JPanel mainPanel;

    public ProgressBarPanel() {
        _description = "";
        getDescriptionLabel().setText(_description);
        this.setLayout(new BorderLayout());
        this.add(getMainPanel());
    }

    private JPanel getMainPanel() {
        if (mainPanel == null) {
            mainPanel = new JPanel();
            mainPanel.setLayout(new GridBagLayout());
            mainPanel.setBackground(Color.WHITE);
            mainPanel.setBorder(javax.swing.BorderFactory.createEtchedBorder(javax.swing.border.EtchedBorder.LOWERED));
            GridBagConstraints gridBagConstraints1 = new GridBagConstraints();
            gridBagConstraints1.gridx = 0;
            gridBagConstraints1.gridy = 0;
            gridBagConstraints1.weightx = 1;
            gridBagConstraints1.weighty = 1;
            gridBagConstraints1.insets = new java.awt.Insets(0, 5, 0, 0);
            gridBagConstraints1.fill = java.awt.GridBagConstraints.HORIZONTAL;
            gridBagConstraints1.anchor = java.awt.GridBagConstraints.WEST;
            gridBagConstraints1.insets = new java.awt.Insets(5, 10, 0, 10);
            mainPanel.add(getDescriptionLabel(), gridBagConstraints1);
            gridBagConstraints1.gridy++;
            mainPanel.add(getJProgressBar(), gridBagConstraints1);
            gridBagConstraints1.gridy++;
            gridBagConstraints1.fill = java.awt.GridBagConstraints.NONE;
            gridBagConstraints1.anchor = java.awt.GridBagConstraints.CENTER;
            mainPanel.add(getCancelButton(), gridBagConstraints1);
        }
        return mainPanel;
    }


    public void changeLoadingText(String description) {
        _description = description;
        getDescriptionLabel().setText(_description);
        this.repaint();
        this.validate();
    }

    public void start() {
        isActive = true;
        _progressValue = 1;
        new Thread(new ProgresoActivo()).start();
        this.getJProgressBar().setVisible(true);
    }

    public void stop() {
        isActive = false;
        _progressValue = 0;
        _description = "";
        _currentThread = null;
        getCancelButton().setVisible(false);
        getDescriptionLabel().setText("");
        getJProgressBar().setVisible(false);
        stateUpdated();
    }

    public void setCurrentProgress(String msg, double progress) {
        isActive = false;
        _progressValue = (int) (100 * progress);
        _description = msg;
        stateUpdated();
    }

    public void stateUpdated() {
        if (_description != null && !_description.isEmpty()) {
            getJProgressBar().setVisible(true);
            getCancelButton().setVisible(showCancelButton);
        }
        getDescriptionLabel().setText(_description);
        if (_progressValue >= 0) {
            getJProgressBar().setValue(_progressValue);
        }
    }

    public void setCurrentThread(Future<?> currentThread) {
        _currentThread = currentThread;
        getCancelButton().setVisible(showCancelButton);
    }

    @Override
    public String getId() {
        return "ProgressBar";
    }

    @Override
    public double getCurrentProgress() {
        return 0; //Generated
    }

    @Override
    public String getCurrentMessage() {
        return null; //Generated
    }

    public Future<?> getCurrentThread() {
        return _currentThread;
    }

    public JButton getCancelButton() {
        if (cancelButton == null) {
            cancelButton = new JButton(OpenEHRLanguageManager.getMessage("Cancel"));
            cancelButton.setIcon(OpenEHRImageUtil.STOP_ICON);
            cancelButton.setBackground(null);
            cancelButton.setBorder(BorderFactory.createEmptyBorder());
            cancelButton.setVisible(false);
            cancelButton.setFocusable(false);
            cancelButton.addActionListener(e -> {
                if (_currentThread != null) {
                    _currentThread.cancel(true);
                } else {
                    LoggerFactory.getLogger(ProgressBarPanel.class).warn("Stopping progress, but currentThread is not set!");
                }
                stop();
            });
        }
        return cancelButton;
    }

    public JLabel getDescriptionLabel() {
        if (descriptionLabel == null) {
            descriptionLabel = new JLabel();
            descriptionLabel.setText("");
        }
        return descriptionLabel;
    }

    public JProgressBar getJProgressBar() {
        if (jProgressBar == null) {
            jProgressBar = new JProgressBar();
            jProgressBar.setName("jProgressBar");
            jProgressBar.setPreferredSize(new java.awt.Dimension(200, 14));
            jProgressBar.setVisible(false);
        }
        return jProgressBar;
    }


    public void setShowCancelButton(boolean showCancelButton) {
        this.showCancelButton = showCancelButton;
    }

    private class ProgresoActivo implements Runnable {
        private boolean up = true;

        public void run() {
            while (isActive) {
                if (up) {
                    _progressValue = _progressValue + 3;
                } else {
                    _progressValue = _progressValue - 3;
                }
                if (_progressValue >= 100) {
                    up = false;
                } else if (_progressValue <= 1) {
                    up = true;
                }
                stateUpdated();
                try {
                    Thread.sleep(50);
                } catch (InterruptedException ex) {
                    log.error("Error waiting in thread",ex);
                }
            }
        }
    }
}
