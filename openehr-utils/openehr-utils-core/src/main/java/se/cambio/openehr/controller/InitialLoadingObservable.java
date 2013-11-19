package se.cambio.openehr.controller;

import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Observable;

public class InitialLoadingObservable extends Observable{

    private static InitialLoadingObservable _instance = null;

    public static enum LoadingStage{
        ARCHETYPES, TEMPLATES, TERMINOLOGIES, ONTOLOGIES, GUIDES
    }

    private LoadingStage _currentLoadingStage = null;
    //From 0 to 1
    private Double _currentProgress = 0.0;
    private int _numLoaded = 0;
    private Collection<InternalErrorException> _loadingExeptions = null;

    private InitialLoadingObservable(){
        _loadingExeptions = new ArrayList<InternalErrorException>();
    }

    public static void setCurrentLoadingStage(LoadingStage loadingStage){
        getDelegate()._currentLoadingStage = loadingStage;
        getDelegate()._currentProgress = 0.0;
        stageUpdated();
    }

    public static void setCurrentLoadingStageFinished(){
        //getDelegate()._currentLoadingStage = null;
        getDelegate()._currentProgress = 0.0;
        getDelegate()._numLoaded++;
        stageUpdated();
    }

    public static void setCurrentProgress(Double currentProgress){
        getDelegate()._currentProgress = currentProgress;
        stageUpdated();
    }

    private static void stageUpdated(){
        getDelegate().setChanged();
        getDelegate().notifyObservers();
    }

    public static LoadingStage getCurrentLoadingStage(){
        return getDelegate()._currentLoadingStage;
    }

    public static Integer getNumLoaded(){
        return getDelegate()._numLoaded;
    }

    public static Double getCurrentStageProgress(){
        return getDelegate()._currentProgress;
    }

    public static void addLoadingException(InternalErrorException e){
        getDelegate()._loadingExeptions.add(e);
    }

    public static Collection<InternalErrorException> getLoadingExceptions(){
        return getDelegate()._loadingExeptions;
    }

    public static InitialLoadingObservable getDelegate(){
        if (_instance==null){
            _instance = new InitialLoadingObservable();
        }
        return _instance;
    }
}
/*
 *  ***** BEGIN LICENSE BLOCK *****
 *  Version: MPL 2.0/GPL 2.0/LGPL 2.1
 *
 *  The contents of this file are subject to the Mozilla Public License Version
 *  2.0 (the 'License'); you may not use this file except in compliance with
 *  the License. You may obtain a copy of the License at
 *  http://www.mozilla.org/MPL/
 *
 *  Software distributed under the License is distributed on an 'AS IS' basis,
 *  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 *  for the specific language governing rights and limitations under the
 *  License.
 *
 *
 *  The Initial Developers of the Original Code are Iago Corbal and Rong Chen.
 *  Portions created by the Initial Developer are Copyright (C) 2012-2013
 *  the Initial Developer. All Rights Reserved.
 *
 *  Contributor(s):
 *
 * Software distributed under the License is distributed on an 'AS IS' basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 *  ***** END LICENSE BLOCK *****
 */