package aviatickets.app.auth;

import java.time.LocalDateTime;
import java.util.Optional;

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
import aviatickets.app.exception.NotFoundException;
import aviatickets.util.HelperService;

@Service
public class AuthService implements AuthInteraction {

  private final HelperService helperService = new HelperService();

  private final CustomerService customerService;
  private final EmailService emailService;
  private final ActionService actionService;

  public AuthService(EmailService emailService, CustomerService customerService, ActionService actionService) {
    this.emailService = emailService;
    this.customerService = customerService;
    this.actionService = actionService;
  }

  public SignInResponse signIn(SignInDto dto) {
    Optional<Customer> customer = customerService.getCustomer(dto.email());
    if (customer.isEmpty())
      throw new NotFoundException("User not found.");
    if (!customer.get().password().equals(dto.password()))
      throw new BadRequestException("Whong email or password.");

    // Token t = jwtService.createToken(customer.get());
    // jwtService.save(t.get().refreshToken());

    return new SignInResponse(); // token pair, customer obj <-
  }

  public void signUp(SignUpDto dto) {
    var customerId = 0;
    customerService.createCustomer(dto.name(), dto.password(), dto.email());
    Optional<Customer> c = customerService.getCustomer(dto.email());
    emailService.sendRegistrationEmail(dto.email());
    if (c.isPresent())
      customerId = c.get().id();
    setActionLog(customerId, dto.email(), "User successfully signed up.");
  }

  public void forgotPassword(String email) {
    String pwd = helperService.generateUniqueString(16);
    Integer customerId = customerService.changePassword(email, pwd);
    emailService.sendForgotPwdEmail(email);
    setActionLog(customerId, email, "User password was changed.");
  }

  // ###
  // -----------------------------------------------------------------------------------
  // ###

  private void setActionLog(Integer customerId, String email, String action) {
    ActionLog a = new ActionLog(
        null,
        email,
        LocalDateTime.now(),
        action,
        customerId);

    actionService.saveCustomerAction(a);
  }

}
