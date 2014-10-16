import org.junit.Test;
import se.cambio.openehr.model.cm.element.dao.GenericCMElementDAO;
import se.cambio.cds.model.orderset.dto.OrderSetDTO;
import se.cambio.openehr.model.util.CMElementDAOFactory;
import se.cambio.openehr.util.exceptions.InstanceNotFoundException;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;

import static org.junit.Assert.assertEquals;

public class PersistenceTest {

    @Test
    public void shouldAllowRounTripForAllCMElements() throws InternalErrorException, InstanceNotFoundException {

        OrderSetDTO orderSetDTO = new OrderSetDTO();
        orderSetDTO.setId("testOrderSetId");
        orderSetDTO.setSource("testSrc");
        orderSetDTO.setLastUpdate(Calendar.getInstance().getTime());
        GenericCMElementDAO<OrderSetDTO> dao = CMElementDAOFactory.getInstance().getDAO(OrderSetDTO.class);
        dao.insert(orderSetDTO);
        Collection<OrderSetDTO> orderSetDTOs = dao.searchAll();
        assertEquals(1, orderSetDTOs.size());

        orderSetDTO = orderSetDTOs.iterator().next();
        assertEquals("testSrc", orderSetDTO.getSource());

        orderSetDTO.setSource("testSrc2");
        dao.update(orderSetDTO);
        orderSetDTOs = dao.searchByIds(Collections.singleton("testOrderSetId"));
        assertEquals(1, orderSetDTOs.size());
        orderSetDTO = orderSetDTOs.iterator().next();
        assertEquals("testSrc2", orderSetDTO.getSource());

        dao.remove("testOrderSetId");
        orderSetDTOs = dao.searchAll();
        assertEquals(0, orderSetDTOs.size());
    }
}
