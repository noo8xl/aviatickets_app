package aviatickets.app.auth;

import java.sql.SQLException;

import aviatickets.app.actions.ActionInterface;
import aviatickets.app.actions.entity.ActionLog;
import aviatickets.app.customer.CustomerInterface;
import aviatickets.app.email.EmailInterface;
import aviatickets.app.jwt.JwtInterface;
import aviatickets.app.util.HelperInterface;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import aviatickets.app.auth.dto.request.SignInDto;
import aviatickets.app.auth.dto.request.SignUpDto;
import aviatickets.app.auth.dto.response.SignInResponse;
import aviatickets.app.customer.entity.Customer;

@RequiredArgsConstructor
@Service
class AuthService implements AuthInterface {

	private final CustomerInterface customerService;
	private final ActionInterface actionService;
	private final EmailInterface emailService;
	private final HelperInterface helperService;

	private final JwtInterface jwtService;

	@Override
  public SignInResponse signIn(SignInDto dto) throws SQLException, ClassNotFoundException {
		Customer c = this.customerService.findOne(dto.email());
	  String token = this.jwtService.generateToken(c);

		// <- print token here only for use it in tests *
		System.out.println("auth token is -> "+ token);
    return new SignInResponse(
				c.getId(),
				c.getName(),
				c.getUsername(),
				c.getIsBanned(),
				c.getTwoStepStatus(),
				token
		);
  }

	@Override
	public Boolean checkTwoStepStatus(String email) throws SQLException, ClassNotFoundException {
		return this.customerService.getTwoStepStatus(email);
	}

	@Override
  public void signUp(SignUpDto dto) throws SQLException, ClassNotFoundException {

		this.customerService.save(dto.name(), dto.password(), dto.email());
		Customer c = this.customerService.findOne(dto.email());
		ActionLog a = this.helperService.setActionLog(c.getUsername(), "User successfully signed up.", c.getId());
		this.actionService.saveLog(a);
		this.emailService.sendRegistrationEmail(c.getUsername());
	}

	@Override
  public void forgotPassword(String email) throws SQLException, ClassNotFoundException {
		Customer c = this.customerService.findOne(email);
		String pwd = this.helperService.generateUniqueString(16);
		this.customerService.updatePassword(email, pwd);
		ActionLog a = this.helperService.setActionLog(email, "Password has been changed.", c.getId());
		this.actionService.saveLog(a);
		this.emailService.sendForgotPwdEmail(email, pwd);
	}

}
