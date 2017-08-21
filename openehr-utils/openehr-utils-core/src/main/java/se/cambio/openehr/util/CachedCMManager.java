package se.cambio.openehr.util;

import se.cambio.cm.model.facade.administration.delegate.ClinicalModelsService;
import se.cambio.cm.model.util.CMElement;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.time.Instant;
import java.util.Calendar;
import java.util.Date;

public class CachedCMManager {

    private static final long MAX_CHECK_WAITING_TIME_IN_MILLIS = 10000; //10 seg
    private Date lastCheckForUpdates;
    private Date lastUpdateDate;
    private Class<? extends CMElement> cmElementClass;
    private ClinicalModelsService clinicalModelsService;

    public CachedCMManager(
            Class<? extends CMElement> cmElementClass,
            ClinicalModelsService clinicalModelsService) {
        this.cmElementClass = cmElementClass;
        this.clinicalModelsService = clinicalModelsService;
        Date currentTime = Calendar.getInstance().getTime();
        lastUpdateDate = currentTime;
        lastCheckForUpdates = currentTime;
    }

    public boolean isDataChangedSince(Instant instant) throws InternalErrorException {
        if (isWaitingTimeForNextCheckReached()) {
            updateLastUpdateDate();
        }
        return lastUpdateDate.after(Date.from(instant));
    }

    private void updateLastUpdateDate() throws InternalErrorException {
        lastUpdateDate = this.clinicalModelsService.getLastUpdate(cmElementClass);
    }

    private boolean isWaitingTimeForNextCheckReached() {
        boolean waitingTimeForNextCheckReached = false;
        long timeSinceLastCheck = System.currentTimeMillis() - lastCheckForUpdates.getTime();
        if (timeSinceLastCheck >= MAX_CHECK_WAITING_TIME_IN_MILLIS) {
            waitingTimeForNextCheckReached = true;
            lastCheckForUpdates = Calendar.getInstance().getTime();
        }
        return waitingTimeForNextCheckReached;
    }

    public void renewLastUpdateDate(Date lastUpdate) {
        if (lastUpdate != null && lastUpdate.after(lastUpdateDate)) {
            lastUpdateDate = lastUpdate;
        }
    }
}
