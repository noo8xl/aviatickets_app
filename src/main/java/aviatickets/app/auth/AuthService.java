package aviatickets.app.auth;

import java.sql.Date;
import java.sql.SQLException;
import java.time.LocalDateTime;

import org.springframework.stereotype.Service;

import aviatickets.app.actions.ActionService;
import aviatickets.app.actions.entity.ActionLog;
import aviatickets.app.auth.dto.request.SignInDto;
import aviatickets.app.auth.dto.request.SignUpDto;
import aviatickets.app.auth.dto.response.SignInResponse;
import aviatickets.app.customer.CustomerService;
import aviatickets.app.customer.entity.Customer;
import aviatickets.app.email.EmailService;
import aviatickets.app.exception.BadRequestException;
import aviatickets.app.util.HelperService;

@Service
public class AuthService implements AuthInteraction {

  private final HelperService helperService = new HelperService();

  private final CustomerService customerService;
  private final EmailService emailService;
  private final ActionService actionService;

  AuthService(EmailService emailService, CustomerService customerService, ActionService actionService) {
    this.emailService = emailService;
    this.customerService = customerService;
    this.actionService = actionService;
  }

	@Override
  public SignInResponse signIn(SignInDto dto) throws SQLException, ClassNotFoundException {

		Boolean status = this.checkTwoStepStatus(dto.email());
		if (Boolean.TRUE.equals(status)) {
			throw new BadRequestException("Two step status is enabled.");
		}

		// Token t = jwtService.createToken(customer.get());
		// jwtService.save(t.get().refreshToken());

    return new SignInResponse(); // token pair, customer obj <-
  }

	@Override
	public Boolean checkTwoStepStatus(String email) throws SQLException, ClassNotFoundException {
		return customerService.getTwoStepStatus(email);
	}

	@Override
  public void signUp(SignUpDto dto) throws SQLException, ClassNotFoundException {
    customerService.createCustomer(dto.name(), dto.password(), dto.email());
    Customer c = customerService.getCustomer(dto.email());
    emailService.sendRegistrationEmail(dto.email());
    setActionLog(c.id(), dto.email(), "User successfully signed up.");
  }

	@Override
  public void forgotPassword(String email) throws SQLException, ClassNotFoundException {
    String pwd = helperService.generateUniqueString(16);
    Integer customerId = customerService.changePassword(email, pwd);
    emailService.sendForgotPwdEmail(email, pwd);
    setActionLog(customerId, email, "User password was changed.");
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

    actionService.saveCustomerAction(a);
  }

}
