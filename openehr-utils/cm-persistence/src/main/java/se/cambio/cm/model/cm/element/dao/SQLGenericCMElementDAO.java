package se.cambio.cm.model.cm.element.dao;

import se.cambio.cm.model.util.CMElement;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import java.lang.reflect.ParameterizedType;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;

public class SQLGenericCMElementDAO<E extends CMElement> implements GenericCMElementDAO<E>{

    private static String CDS_PERSISTENCE_UNIT = "cdsPU";
    private EntityManagerFactory emf;

    @Override
    public Collection<E> searchByIds(Collection<String> ids)
            throws InternalErrorException, InstanceNotFoundException {
        if (ids==null || ids.isEmpty()){
            return new ArrayList<E>();
        }
        EntityManager em = null;
        try{
            em = getEntityManagerFactory().createEntityManager();
            Collection<E> cmElements = (Collection<E>)em.createQuery("SELECT e FROM "+ getCMElementClassName()+" e WHERE e.id in (:ids)").setParameter("ids", ids).getResultList();
            if (cmElements.size()<ids.size()){
                checkMissingInstance(ids, cmElements);
            }
            return cmElements;
        }catch(Exception e){
            throw new InternalErrorException(e);
        }finally {
            if (em!=null){
                em.close();
            }
        }
    }

    private void checkMissingInstance(Collection<String> ids, Collection<E> cmElements) throws InstanceNotFoundException {
        Collection<String> foundIds = new ArrayList<String>();
        for (CMElement cmElement: cmElements){
            foundIds.add(cmElement.getId());
        }
        for(String id: ids){
            if (!foundIds.contains(id)){
                throw new InstanceNotFoundException(id, getCMElementClassName());
            }
        }
    }

    @Override
    public Collection<E> searchAll() throws InternalErrorException {
        EntityManager em = null;
        try{
            em = getEntityManagerFactory().createEntityManager();
            return (Collection<E>)em.createQuery("SELECT e FROM "+ getCMElementClassName()+" e").getResultList();
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
            return (Collection<String>)em.createQuery("SELECT e.id FROM " + getCMElementClassName() + " e").getResultList();
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
            E cmElement = em.find(getCMElementClass(), id);
            if (cmElement==null){
                throw new InstanceNotFoundException(id, getCMElementClassName());
            }
            em.getTransaction().begin();
            em.remove(cmElement);
            em.getTransaction().commit();
        }finally {
            if (em!=null){
                em.close();
            }
        }
    }

    @Override
    public void removeAll() throws InternalErrorException {
        EntityManager em = null;
        try{
            em = getEntityManagerFactory().createEntityManager();
            em.getTransaction().begin();
            em.createQuery("DELETE FROM "+ getCMElementClassName()+" e").executeUpdate();
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
            List dates = em.createQuery("SELECT MAX(e.lastUpdate) FROM "+ getCMElementClassName()+" e").getResultList();
            if (dates.isEmpty()){
                return null;
            }
            Object dateObj = dates.iterator().next();
            if (!(dateObj instanceof Date)){
                throw new InternalErrorException(new Exception("Expected date class, found '"+dateObj.getClass().getName()+"' instead."));
            }
            return (Date)dateObj;
        }catch(Exception e){
            throw new InternalErrorException(e);
        }finally {
            if (em!=null){
                em.close();
            }
        }
    }

    private String getCMElementClassName() {
        return getCMElementClass().getSimpleName();
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
