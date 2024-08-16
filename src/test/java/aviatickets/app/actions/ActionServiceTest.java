package aviatickets.app.actions;


import aviatickets.app.actions.entity.ActionLog;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.sql.SQLException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertNotNull;

@SpringBootTest(classes = ActionService.class)
class ActionServiceTest {

	private final Logger log = LoggerFactory.getLogger(ActionServiceTest.class);

	@MockBean
	ActionInterface actionService;

	@Test
	void saveLog(){
		log.info("saveLog is OK.");
	}

	@Test
	void getLog() throws SQLException, ClassNotFoundException {
		List<ActionLog> logList = this.actionService.getLog(0,10,1);

		assertNotNull(logList);
		log.info("getLog is OK.");
	}
}