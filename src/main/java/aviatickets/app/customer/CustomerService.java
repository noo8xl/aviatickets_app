package aviatickets.app.customer;

import java.sql.SQLException;
import java.util.List;
import java.util.Objects;

import aviatickets.app.actions.ActionInterface;
import aviatickets.app.actions.entity.ActionLog;
import aviatickets.app.customer.dto.ChangeTwoStepStatusDto;
import aviatickets.app.customer.dto.UpdateCustomerDto;
import aviatickets.app.notification.NotificationInterface;
import aviatickets.app.notification.dto.NotificationDto;
import aviatickets.app.util.HelperInterface;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

import aviatickets.app.customer.entity.Customer;
import aviatickets.app.exception.ServerErrorException;

@RequiredArgsConstructor
@Service
public class CustomerService implements CustomerInterface {

  private static final Logger log = LoggerFactory.getLogger(CustomerService.class);
  private final CustomerRepository customerRepository; // ->
	private final NotificationInterface notificationService;
	private final HelperInterface helperService;
	private final ActionInterface actionService;

	@Override
  public void save(String name, String password, String email) {
		try {
			this.customerRepository.save(name, email, password);
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
  }

	@Override
	@Cacheable("customer")
	public Customer findOne(Integer id) {
		try {
			return this.customerRepository.findOne(id);
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}

	}

	@Override
	@Cacheable("customer")
  public Customer findOne(String email) {
		try {
			return this.customerRepository.findOne(email);
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
  }

	@Override
  public void updateProfile(UpdateCustomerDto dto) {
		try {
			this.customerRepository.updateProfile(dto);
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
  }

	@Override
  public void updatePassword(String email, String pwd) {
		try {
			this.customerRepository.updatePassword(email, pwd);
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
  }

	@Override
  public void deleteCustomer(Integer idToDelete, Integer adminId) {
		try {
			this.customerRepository.deleteCustomer(idToDelete, adminId);
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
  }

	@Override
	public void update2faStatus(ChangeTwoStepStatusDto dto) {

		ActionLog a;
		NotificationDto notifDto;
		String customerAction;

		try {
			customerAction = String
				.format("Customer 2fa status was changed! Current status is: %s.", dto.status().toString());
			this.customerRepository.update2faStatus(dto);
			a = this.helperService.setActionLog(
					dto.email(),
					customerAction,
					dto.customerId());
			this.actionService.saveLog(a);

			boolean isEqual = Boolean.TRUE.equals(Objects.equals(dto.type(), "telegram"));

			log.info("isEqual -> {}", isEqual);

			if (isEqual) {
				notifDto = new NotificationDto("telegram", "2fa was enabled", dto.telegramId());
			} else {
				notifDto = new NotificationDto("email", "2fa was enabled", dto.email());
			}
			this.notificationService.sendCustomNotification(notifDto);
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
	}

	@Override
	public Boolean getTwoStepStatus(String email) {
		try {
			return this.customerRepository.getTwoStepStatus(email);
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
	}

	@Override
	public String getTwoStepStatusType(String email) throws SQLException, ClassNotFoundException {
		return this.customerRepository.getTwoStepStatusType(email);
	}

	@Override
	public String getCustomerTelegramId(String email) throws SQLException, ClassNotFoundException {
		return this.customerRepository.getCustomerTelegramId(email);
	}

@Override
	public List<Customer> findAll(Integer skip, Integer limit) {
		try {
			return this.customerRepository.findAll(skip, limit);
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
	}

	@Override
	public void updateBanStatus(Integer customerId, Boolean status) {
		try {
			this.customerRepository.updateBanStatus(customerId, status);
		} catch (Exception e) {
			throw new ServerErrorException(e.getMessage());
		}
	}



}






