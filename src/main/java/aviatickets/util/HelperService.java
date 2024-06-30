package aviatickets.util;

import java.util.Random;

public class HelperService {
  private final Random random = new Random();

  // generateUniqueString -> geerate new pwd OR 2fa code
  public String generateUniqueString(Integer len) {
    int leftLimit = 97; // letter 'a'
    int rightLimit = 122; // letter 'z'
    StringBuilder buffer = new StringBuilder(len);
    for (int i = 0; i < len; i++) {
      int randomLimitedInt = leftLimit + (this.random.nextInt() * (rightLimit - leftLimit + 1));
      buffer.append((char) randomLimitedInt);
    }
    return buffer.toString();
  }
}
