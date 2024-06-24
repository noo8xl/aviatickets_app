package aviaTickets.util;

import java.util.Random;

import org.springframework.stereotype.Service;

@Service
public class HelperService {
  
  // generateUniqueString -> geerate new pwd OR 2fa code
  public String generateUniqueString(Integer len) {
    int leftLimit = 97; // letter 'a'
    int rightLimit = 122; // letter 'z'
    Random random = new Random();
    StringBuilder buffer = new StringBuilder(len);
    for (int i = 0; i < len; i++) {
        int randomLimitedInt = leftLimit + (int) 
          (random.nextFloat() * (rightLimit - leftLimit + 1));
        buffer.append((char) randomLimitedInt);
    }
    String generatedString = buffer.toString();
    return generatedString;
  }
}
