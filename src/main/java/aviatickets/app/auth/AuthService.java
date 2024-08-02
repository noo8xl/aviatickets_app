package aviatickets.app.auth;


import java.sql.SQLException;

import aviatickets.app.actions.entity.ActionLog;
import aviatickets.app.jwt.JwtService;
import org.springframework.stereotype.Service;

import aviatickets.app.actions.ActionService;
import aviatickets.app.auth.dto.request.SignInDto;
import aviatickets.app.auth.dto.request.SignUpDto;
import aviatickets.app.auth.dto.response.SignInResponse;
import aviatickets.app.customer.CustomerService;
import aviatickets.app.customer.entity.Customer;
import aviatickets.app.email.EmailService;
import aviatickets.app.util.HelperService;

@Service
class AuthService implements AuthInteraction {

	private final CustomerService customerService;
	private final EmailService emailService;
	private final ActionService actionService;
	private final HelperService helperService;
	private final JwtService jwtService;

  AuthService(
			EmailService emailService, CustomerService customerService,
			ActionService actionService, HelperService helperService,
			JwtService jwtService
	) {
    this.emailService = emailService;
    this.customerService = customerService;
    this.actionService = actionService;
		this.helperService = helperService;
		this.jwtService = jwtService;
	}


	@Override
  public SignInResponse signIn(SignInDto dto) throws SQLException, ClassNotFoundException {
		Customer c = this.customerService.findOne(dto.email());
	  String token = this.jwtService.generateToken(c);

		// <- print token here only for use it in tests *
		System.out.println("auth token is -> "+ token);
    return new SignInResponse(
				c.getCustomerId(),
				c.getCustomerName(),
				c.getUsername(),
				c.getBanStatus(),
				c.get2faStatus(),
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
		ActionLog a = this.helperService.setActionLog(c.getUsername(), "User successfully signed up.", c.getCustomerId());
		this.actionService.saveCustomerAction(a);
		this.emailService.sendRegistrationEmail(c.getUsername());
	}

	@Override
  public void forgotPassword(String email) throws SQLException, ClassNotFoundException {
    String pwd = this.helperService.generateUniqueString(16);
    Integer customerId = this.customerService.updatePassword(email, pwd);
		ActionLog a = this.helperService.setActionLog(email, "Password has been changed.", customerId);
		this.actionService.saveCustomerAction(a);
		this.emailService.sendForgotPwdEmail(email, pwd);
	}

}
