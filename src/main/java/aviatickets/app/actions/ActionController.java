package aviatickets.app.actions;

import aviatickets.app.actions.entity.ActionLog;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.sql.SQLException;
import java.util.List;

@RequiredArgsConstructor
@RestController
@RequestMapping("/action")
public class ActionController {

	private final ActionService actionService;

// available ONLY for ADMIN *
	@ResponseStatus(HttpStatus.OK)
	@GetMapping("/get-action-list/{skip}/{limit}/{customerId}/")
	public ResponseEntity<List<ActionLog>>  getActionList(
			@PathVariable Integer skip,
			@PathVariable Integer limit,
			@PathVariable Integer customerId
	) throws SQLException, ClassNotFoundException {
			return ResponseEntity.ok(this.actionService.getLog(skip, limit, customerId));
	}
}
