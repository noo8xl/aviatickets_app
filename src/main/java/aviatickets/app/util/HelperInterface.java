package aviatickets.app.util;

import aviatickets.app.actions.entity.ActionLog;

public interface HelperInterface {

	String generateUniqueString(Integer len);

	ActionLog setActionLog(String email, String action, Integer customerId);
}
