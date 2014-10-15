package se.cambio.cds.model.cm.element.dao;

import se.cambio.cds.model.kb.instance.dto.KBInstanceDTO;
import se.cambio.openehr.model.util.CMElement;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import java.lang.reflect.ParameterizedType;
import java.util.Collection;
import java.util.Date;

public abstract class SQLGenericCMElementDAO<E extends CMElement> implements GenericCMElementDAO<E>{

    private static String CDS_PERSISTENCE_UNIT = "cdsPU";
    private EntityManagerFactory emf;

    @Override
    public Collection<E> searchByIds(Collection<String> ids)
            throws InternalErrorException, InstanceNotFoundException {
        EntityManager em = null;
        try{
            em = getEntityManagerFactory().createEntityManager();
            return (Collection<E>)em.createQuery("SELECT e FROM "+getCMElementClass()+" e WHERE e.id in (:ids)").setParameter("ids", ids).getResultList();
        }catch(Exception e){
            throw new InternalErrorException(e);
        }finally {
            if (em!=null){
                em.close();
            }
        }
    }

    @Override
    public Collection<E> searchAll() throws InternalErrorException {
        EntityManager em = null;
        try{
            em = getEntityManagerFactory().createEntityManager();
            return (Collection<E>)em.createQuery("SELECT e FROM "+getCMElementClass()+" e").getResultList();
        }catch(Exception e){
            throw new InternalErrorException(e);
        }finally {
            if (em!=null){
                em.close();
            }
        }
    }

    @Override
    public Collection<String> searchAllIds() throws InternalErrorException {
        EntityManager em = null;
        try{
            em = getEntityManagerFactory().createEntityManager();
            return (Collection<String>)em.createQuery("SELECT e.id FROM " + getCMElementClass() + " e").getResultList();
        }catch(Exception e){
            throw new InternalErrorException(e);
        }finally {
            if (em!=null){
                em.close();
            }
        }
    }

    @Override
    public void insert(E cmElementDTO) throws InternalErrorException {
        upsert(cmElementDTO);
    }

    @Override
    public void update(E cmElementDTO) throws InternalErrorException {
        upsert(cmElementDTO);
    }

    private void upsert(E cmElementDTO) throws InternalErrorException  {
        EntityManager em = null;
        try{
            em = getEntityManagerFactory().createEntityManager();
            em.getTransaction().begin();
            em.merge(cmElementDTO);
            em.getTransaction().commit();
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
            em = getEntityManagerFactory().createEntityManager();
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
            em = getEntityManagerFactory().createEntityManager();
            return (Date)em.createQuery("SELECT MAX(e.lastUpdate) FROM "+getCMElementClass()+" e").getResultList();
        }catch(Exception e){
            throw new InternalErrorException(e);
        }finally {
            if (em!=null){
                em.close();
            }
        }
    }

    private Class<E> getCMElementClass() {
        return (Class<E>) ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[0];
    }

    public EntityManagerFactory getEntityManagerFactory() {
        if (emf == null) {
            emf = Persistence.createEntityManagerFactory(CDS_PERSISTENCE_UNIT);
        }
        return emf;
    }
}
