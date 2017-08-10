package se.cambio.cm.model.util;

import se.cambio.cm.model.util.comparators.CMElementComparator;
import se.cambio.openehr.util.exceptions.InternalErrorException;

import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class CheckSumManager {

    public static <E extends CMElement> String generateChecksum(Collection<E> cmElements) throws InternalErrorException {
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            List<E> cmElementList = getSortedCMElementList(cmElements);
            return getMD5Checksum(md, cmElementList);
        } catch (NoSuchAlgorithmException e) {
            throw new InternalErrorException(e);
        }
    }

    private static <E extends CMElement> String getMD5Checksum(MessageDigest md, List<E> cmElementList) {
        for (E cmElement : cmElementList) {
            try {
                md.update(cmElement.getSource().getBytes("UTF-8"));
            } catch (UnsupportedEncodingException e) {
                throw new RuntimeException(e);
            }
        }
        byte[] md5Bytes = md.digest();
        StringBuilder sb = new StringBuilder();
        for (byte md5Byte : md5Bytes) {
            sb.append(Integer.toString((md5Byte & 0xff) + 0x100, 16).substring(1));
        }
        return sb.toString();
    }

    private static <E extends CMElement> List<E> getSortedCMElementList(Collection<E> cmElements) {
        List<E> cmElementList = new ArrayList<>();
        cmElementList.addAll(cmElements);
        cmElementList.sort(new CMElementComparator());
        return cmElementList;
    }
}
