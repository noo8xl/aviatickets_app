package aviatickets.app.auth;

import aviatickets.app.auth.dto.request.SignUpDto;
import com.google.zxing.WriterException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.io.IOException;
import java.sql.SQLException;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.times;

@SpringBootTest(classes = AuthService.class)
class AuthServiceTest {

	private final Logger log = LoggerFactory.getLogger(AuthServiceTest.class);

	@MockBean
	private AuthInterface repo;

	private SignUpDto signUpDto;



	@BeforeEach
	void setUp() {
		this.signUpDto = new SignUpDto(
			"user1",
			"email@example.com",
			"qwerty123456"
		);
	}

	@Test
	void signUp() throws SQLException, IOException, ClassNotFoundException, WriterException {
		this.repo.signUp(this.signUpDto);

		Mockito.verify(this.repo, times(1)).signUp(this.signUpDto);
		log.info("signUp test is OK");
	}

	@Test
	void signIn() {
//		this.repo.signIn();
	}

	@Test
	void checkTwoStepStatus() {
	}



	@Test
	void forgotPassword() {
	}
}