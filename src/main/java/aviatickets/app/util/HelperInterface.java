package aviatickets.app.util;

import aviatickets.app.actions.entity.ActionLog;

public interface HelperInterface {
// generate 2fa code or new password here
	String generateUniqueString(Integer len);

// set log entity
	ActionLog setActionLog(String email, String action, Integer customerId);
}
