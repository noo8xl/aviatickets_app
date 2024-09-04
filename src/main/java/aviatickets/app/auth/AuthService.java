package aviatickets.app.auth;

import aviatickets.app.actions.ActionInterface;
import aviatickets.app.actions.entity.ActionLog;
import aviatickets.app.customer.CustomerInterface;
import aviatickets.app.exception.ServerErrorException;
import aviatickets.app.jwt.JwtInterface;
import aviatickets.app.notification.NotificationInterface;
import aviatickets.app.notification.dto.NotificationDto;
import aviatickets.app.util.HelperInterface;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import aviatickets.app.auth.dto.request.SignInDto;
import aviatickets.app.auth.dto.request.SignUpDto;
import aviatickets.app.auth.dto.response.SignInResponse;
import aviatickets.app.customer.entity.Customer;

import java.sql.SQLException;

@RequiredArgsConstructor
@Service
class AuthService implements AuthInterface {

	private final CustomerInterface customerService;
	private final ActionInterface actionService;
	private final NotificationInterface notificationService;
	private final HelperInterface helperService;

	private final JwtInterface jwtService;

	@Override
  public SignInResponse signIn(SignInDto dto) {

		Customer c;
		String token;
		SignInResponse resp;
		Boolean twoStepStatus;
		ActionLog a;

		try {

			c = this.customerService.findOne(dto.email());
			twoStepStatus = this.customerService.getTwoStepStatus(dto.email());

			if(Boolean.TRUE.equals(twoStepStatus)) {
				this.sendTwoStepCodeToTheCustomer(dto.email());
				return null;
			} else {
				token = this.jwtService.generateToken(c);

				resp = new SignInResponse(
						c.getId(),
						c.getName(),
						c.getUsername(),
						c.getIsBanned(),
						c.getTwoStepStatus(),
						token
				);
			}

			a = this.helperService.setActionLog(c.getUsername(), "User successfully signed in.", c.getId());
			this.actionService.saveLog(a);

		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
		return resp;
  }

	@Override
	public Boolean checkTwoStepStatus(String email) throws SQLException, ClassNotFoundException {
		return this.customerService.getTwoStepStatus(email);
	}

	@Override
	public void sendTwoStepCodeToTheCustomer(String email) {

		String code;
		NotificationDto notificationDto;
		String type;
		String telegramId;

		try {

			code = this.helperService.generateUniqueString(8);
			type = this.customerService.getTwoStepStatusType(email);

			if(type.equalsIgnoreCase("email"))
				notificationDto = new NotificationDto("email", code, email);
			else {
				telegramId = this.customerService.getCustomerTelegramId(email);
				notificationDto = new NotificationDto(type, code, telegramId);
			}

			this.notificationService.sendTwoStepCode(notificationDto);
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
	}

	@Override
  public void signUp(SignUpDto dto) {

		Customer c;
		ActionLog a;

		try {
			this.customerService.save(dto.name(), dto.password(), dto.email());
			c = this.customerService.findOne(dto.email());
			a = this.helperService.setActionLog(c.getUsername(), "User successfully signed up.", c.getId());
			this.actionService.saveLog(a);
			this.notificationService.sendRegistrationEmail(c.getUsername());
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
	}

	@Override
  public void forgotPassword(String email) {

		Customer c;
		ActionLog a;
		String pwd;
		NotificationDto dto;

		try {
			c = this.customerService.findOne(email);
			pwd = this.helperService.generateUniqueString(16);
			this.customerService.updatePassword(email, pwd);
			a = this.helperService.setActionLog(email, "Password has been changed.", c.getId());
			this.actionService.saveLog(a);
			dto = new NotificationDto("email", pwd, email);
			this.notificationService.sendNewPwd(dto);
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}

	}

}
