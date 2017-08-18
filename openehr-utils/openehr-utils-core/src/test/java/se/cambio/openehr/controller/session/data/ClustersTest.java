package se.cambio.openehr.controller.session.data;

import org.testng.annotations.Test;
import se.cambio.cm.model.archetype.vo.ClusterVO;

import java.util.Arrays;
import java.util.Collections;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
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
                        ClusterVO.builder()
                                .description("elementIdTest1")
                                .type("DV_TEXT")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/clusterPath1")
                                .build(),
                        ClusterVO.builder()
                                .description("elementIdTest2")
                                .type("DV_TEXT")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/clusterPath2")
                                .build()));
        ClusterVO clusterVO = clusters.getClusterVO(null, TEST_ARCHETYPE_ID + "/clusterPath1");
        assertThat(clusterVO.getDescription(), equalTo("elementIdTest1"));
        clusterVO = clusters.getClusterVO(null, TEST_ARCHETYPE_ID + "/clusterPath2");
        assertThat(clusterVO.getDescription(), equalTo("elementIdTest2"));

    }

    @Test
    public void should_not_find_cluster() {
        Clusters clusters = new Clusters(mock(ArchetypeManager.class));
        ClusterVO clusterVO = clusters.getClusterVO(null, TEST_ARCHETYPE_ID + "/clusterPath1");
        assertThat(clusterVO, is(nullValue()));
    }

    @Test
    public void should_find_cluster_after_second_load() {
        Clusters clusters = new Clusters(mock(ArchetypeManager.class));
        clusters.loadClusters(
                TEST_ARCHETYPE_ID,
                null,
                Collections.singletonList(
                        ClusterVO.builder()
                                .description("elementIdTest1")
                                .type("DV_TEXT")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/clusterPath1")
                                .build()));
        clusters.loadClusters(
                TEST_ARCHETYPE_ID,
                null,
                Collections.singletonList(
                        ClusterVO.builder()
                                .description("elementIdTest2")
                                .type("DV_TEXT")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/clusterPath2")
                                .build()));
        ClusterVO clusterVO = clusters.getClusterVO(null, TEST_ARCHETYPE_ID + "/clusterPath2");
        assertThat(clusterVO.getDescription(), equalTo("elementIdTest2"));
    }

    @Test
    public void should_not_find_cluster_after_second_load() {
        Clusters clusters = new Clusters(mock(ArchetypeManager.class));
        clusters.loadClusters(
                TEST_ARCHETYPE_ID,
                null,
                Collections.singletonList(
                        ClusterVO.builder()
                                .description("elementIdTest1")
                                .type("DV_TEXT")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/clusterPath1")
                                .build()));
        clusters.loadClusters(
                TEST_ARCHETYPE_ID,
                null,
                Collections.singletonList(
                        ClusterVO.builder()
                                .description("elementIdTest2")
                                .type("DV_TEXT")
                                .idArchetype(TEST_ARCHETYPE_ID)
                                .path("/clusterPath2")
                                .build()));
        ClusterVO clusterVO = clusters.getClusterVO(null, TEST_ARCHETYPE_ID + "/clusterPath1");
        assertThat(clusterVO, is(nullValue()));
    }
}