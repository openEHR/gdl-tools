package se.cambio.cds.model.kb.instance.dao;

import se.cambio.cds.model.kb.instance.dto.KBInstanceDTO;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import java.util.Collection;
import java.util.Date;

/**
 * @author iago.corbal
 */
public class SQLGenericKBInstanceDAO implements GenericKBInstanceDAO{

    EntityManagerFactory emf;
    public SQLGenericKBInstanceDAO() throws InternalErrorException {
        emf = Persistence.createEntityManagerFactory("cdsPU");
    }

    @Override
    public Collection<KBInstanceDTO> searchByIds(Collection<String> ids)
            throws InternalErrorException, InstanceNotFoundException {
        EntityManager em = null;
        try{
            em = emf.createEntityManager();
            return (Collection<KBInstanceDTO>)em.createQuery("SELECT e FROM KBInstanceDTO e WHERE e.kbInstanceId in (:ids)").setParameter("ids", ids).getResultList();
        }catch(Exception e){
            throw new InternalErrorException(e);
        }finally {
            if (em!=null){
                em.close();
            }
        }
    }

    @Override
    public Collection<KBInstanceDTO> searchAll() throws InternalErrorException {
        EntityManager em = null;
        try{
            em = emf.createEntityManager();
            return (Collection<KBInstanceDTO>)em.createQuery("SELECT e FROM KBInstanceDTO e").getResultList();
        }catch(Exception e){
            throw new InternalErrorException(e);
        }finally {
            if (em!=null){
                em.close();
            }
        }
    }

    @Override
    public KBInstanceDTO upsert(KBInstanceDTO kbInstanceDTO) throws InternalErrorException  {
        EntityManager em = null;
        try{
            em = emf.createEntityManager();
            em.getTransaction().begin();
            kbInstanceDTO = em.merge(kbInstanceDTO);
            em.getTransaction().commit();
            return kbInstanceDTO;
        }finally {
            if (em!=null){
                em.close();
            }
        }
    }

    @Override
    public void remove(String id) throws InternalErrorException, InstanceNotFoundException {
        EntityManager em = null;
        try{
            em = emf.createEntityManager();
            KBInstanceDTO kbInstanceDTO = em.find(KBInstanceDTO.class, id);
            em.getTransaction().begin();
            em.remove(kbInstanceDTO);
            em.getTransaction().commit();
        }finally {
            if (em!=null){
                em.close();
            }
        }
    }

    @Override
    public Date getLastUpdateDate() throws InternalErrorException {
        EntityManager em = null;
        try{
            em = emf.createEntityManager();
            return (Date)em.createQuery("SELECT MAX(e.lastUpdate) FROM KBInstanceDTO e").getResultList();
        }catch(Exception e){
            throw new InternalErrorException(e);
        }finally {
            if (em!=null){
                em.close();
            }
        }
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