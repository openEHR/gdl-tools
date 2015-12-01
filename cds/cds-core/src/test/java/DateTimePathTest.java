import org.hamcrest.CoreMatchers;
import org.joda.time.DateTime;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.openehr.rm.datatypes.basic.DataValue;
import org.openehr.rm.datatypes.quantity.datetime.DvDateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import se.cambio.cds.model.instance.ArchetypeReference;
import se.cambio.cds.model.instance.ElementInstance;
import se.cambio.cds.util.DateTimeARFinder;
import se.cambio.cds.util.Domains;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = DateTimeARFinder.class)
public class DateTimePathTest {
    @Autowired
    DateTimeARFinder dateTimeARFinder;

    @Test
    public void should_find_date_time_paths() {
        ArchetypeReference ar = new ArchetypeReference(Domains.CDS_ID, "openEHR-EHR-EVALUATION.problem-diagnosis.v1", null);
        String dateStr = "2015-01-01T12:01:01";
        DateTime originalDateTime = new DateTime(dateStr);
        DataValue dv = new DvDateTime(dateStr);
        new ElementInstance("openEHR-EHR-EVALUATION.problem-diagnosis.v1/data[at0001]/items[at0003]", dv, ar, null, null);
        DateTime dateTimeResult = dateTimeARFinder.getDateTime(ar);
        Assert.assertThat(originalDateTime, CoreMatchers.equalTo(dateTimeResult));
    }
}
