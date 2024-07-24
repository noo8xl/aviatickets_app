package aviatickets.app.actions;

import java.sql.SQLException;
import java.util.List;

import aviatickets.app.actions.entity.ActionLog;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/action")
public class ActionController {

	private final ActionService actionService;

	public ActionController(ActionService actionService) {
		this.actionService = actionService;
	}

	// available ONLY for admin user *
	@ResponseStatus(HttpStatus.OK)
	@GetMapping("/get-action-list/{skip}/{limit}/{customerId}/{adminId}/")
	public ResponseEntity<List<ActionLog>>  getActionList(
			@PathVariable Integer skip, @PathVariable Integer limit,
			@PathVariable Integer customerId, @PathVariable Integer adminId) throws SQLException, ClassNotFoundException {
			return ResponseEntity.ok(this.actionService.getCustomerLog(skip, limit, customerId, adminId));
	}
}
