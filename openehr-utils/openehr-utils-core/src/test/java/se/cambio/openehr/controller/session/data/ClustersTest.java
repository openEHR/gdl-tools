package se.cambio.openehr.controller.session.data;

import org.testng.annotations.Test;
import se.cambio.cm.model.archetype.vo.ClusterVO;

import java.util.Arrays;
import java.util.Collections;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.mock;

public class ClustersTest {

    private static final String TEST_ARCHETYPE_ID = "testArchetypeId";


    @Test
    public void should_find_cluster_in_element_id() {
        Clusters clusters = new Clusters(mock(ArchetypeManager.class));
        clusters.loadClusters(
                TEST_ARCHETYPE_ID,
                null,
                Arrays.asList(
                        new ClusterVO(null, "elementIdTest1", "DV_TEXT", TEST_ARCHETYPE_ID, null, "/clusterPath1"),
                        new ClusterVO(null, "elementIdTest2", "DV_TEXT", TEST_ARCHETYPE_ID, null, "/clusterPath2")));
        ClusterVO clusterVO = clusters.getClusterVO(null, TEST_ARCHETYPE_ID + "/clusterPath1");
        assertThat(clusterVO.getDescription(), equalTo("elementIdTest1"));
        clusterVO = clusters.getClusterVO(null, TEST_ARCHETYPE_ID + "/clusterPath2");
        assertThat(clusterVO.getDescription(), equalTo("elementIdTest2"));

    }

    @Test(expectedExceptions = RuntimeException.class)
    public void should_not_find_cluster() {
        Clusters clusters = new Clusters(mock(ArchetypeManager.class));
        clusters.getClusterVO(null, TEST_ARCHETYPE_ID + "/clusterPath1");
    }

    @Test
    public void should_find_cluster_after_second_load() {
        Clusters clusters = new Clusters(mock(ArchetypeManager.class));
        clusters.loadClusters(
                TEST_ARCHETYPE_ID,
                null,
                Collections.singletonList(
                        new ClusterVO(null, "elementIdTest1", "DV_TEXT", TEST_ARCHETYPE_ID, null, "/clusterPath1")));
        clusters.loadClusters(
                TEST_ARCHETYPE_ID,
                null,
                Collections.singletonList(
                        new ClusterVO(null, "elementIdTest2", "DV_TEXT", TEST_ARCHETYPE_ID, null, "/clusterPath2")));
        ClusterVO clusterVO = clusters.getClusterVO(null, TEST_ARCHETYPE_ID + "/clusterPath2");
        assertThat(clusterVO.getDescription(), equalTo("elementIdTest2"));
    }

    @Test(expectedExceptions = RuntimeException.class)
    public void should_not_find_cluster_after_second_load() {
        Clusters clusters = new Clusters(mock(ArchetypeManager.class));
        clusters.loadClusters(
                TEST_ARCHETYPE_ID,
                null,
                Collections.singletonList(
                        new ClusterVO(null, "elementIdTest1", "DV_TEXT", TEST_ARCHETYPE_ID, null, "/clusterPath1")));
        clusters.loadClusters(
                TEST_ARCHETYPE_ID,
                null,
                Collections.singletonList(
                        new ClusterVO(null, "elementIdTest2", "DV_TEXT", TEST_ARCHETYPE_ID, null, "/clusterPath2")));
        clusters.getClusterVO(null, TEST_ARCHETYPE_ID + "/clusterPath1");
    }
}