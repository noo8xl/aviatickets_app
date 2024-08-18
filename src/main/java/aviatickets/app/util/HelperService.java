package aviatickets.app.util;

import aviatickets.app.actions.entity.ActionLog;

import lombok.NoArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Random;

@Component
@NoArgsConstructor
public class HelperService implements HelperInterface {

	@Override
  public String generateUniqueString(Integer len) {
		String charList = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890";
		StringBuilder str = new StringBuilder();
		Random rnd = new Random();
		while (str.length() < len) { // length of the random string.
			int index = (int) (rnd.nextFloat() * charList.length());
			str.append(charList.charAt(index));
		}
		return str.toString();
  }


	@Override
	public ActionLog setActionLog(String email, String action, Integer customerId) {
		ActionLog a = new ActionLog();
		a.setAction(null, email, null, action, customerId);
		return a.getActionEntity();
	}

// ####################################################################################################
// ################################ get entity from sql.resultSet area ################################
// ####################################################################################################



// ####################################################################################################
// #################################### end of sql.resultSet area #####################################
// ####################################################################################################


}
