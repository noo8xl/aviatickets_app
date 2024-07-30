package aviatickets.app.auth;

import java.sql.Date;
import java.sql.SQLException;

import aviatickets.app.jwt.JwtService;
import org.springframework.stereotype.Service;

import aviatickets.app.actions.ActionService;
import aviatickets.app.actions.entity.ActionLog;
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
		this.customerService.isCustomerExists(dto.email());
		Customer c = this.customerService.getCustomer(dto.email());
	  String t = this.jwtService.generateToken(c);
		SignInResponse response = new SignInResponse(c, t).getResponse();

		System.out.println("resp is -> "+ response.toString());
    return response;
  }

	@Override
	public Boolean checkTwoStepStatus(String email) throws SQLException, ClassNotFoundException {
		return this.customerService.getTwoStepStatus(email);
	}

	@Override
  public void signUp(SignUpDto dto) throws SQLException, ClassNotFoundException {
    this.customerService.createCustomer(dto.name(), dto.password(), dto.email());
    Customer c = this.customerService.getCustomer(dto.email());
    this.emailService.sendRegistrationEmail(dto.email());
    this.setActionLog(c.getCustomerId(), dto.email(), "User successfully signed up.");
  }

	@Override
  public void forgotPassword(String email) throws SQLException, ClassNotFoundException {
    String pwd = this.helperService.generateUniqueString(16);
    Integer customerId = this.customerService.changePassword(email, pwd);
    this.emailService.sendForgotPwdEmail(email, pwd);
    this.setActionLog(customerId, email, "User password was changed.");
  }

  // -----------------------------------------------------------------------------------

	// setActionLog -> create new ActionLog entity and save it to db
  private void setActionLog(Integer customerId, String email, String action) throws SQLException, ClassNotFoundException {
    ActionLog a = new ActionLog(
        null,
        email,
        new Date(System.currentTimeMillis()),
        action,
        customerId
		);

    this.actionService.saveCustomerAction(a);
  }

}
