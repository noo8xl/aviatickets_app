package aviatickets.app.customer;

import java.sql.SQLException;
import java.util.List;

import aviatickets.app.actions.ActionInterface;
import aviatickets.app.actions.entity.ActionLog;
import aviatickets.app.customer.dto.ChangeTwoStepStatusDto;
import aviatickets.app.customer.dto.UpdateCustomerDto;
import aviatickets.app.email.EmailInterface;
import aviatickets.app.util.HelperInterface;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;

import aviatickets.app.customer.entity.Customer;
import aviatickets.app.exception.ServerErrorException;

@RequiredArgsConstructor
@Service
public class CustomerService implements CustomerInterface {

//  private static final Logger log = LoggerFactory.getLogger(CustomerService.class);
  private final CustomerRepository customerRepository; // ->
	private final EmailInterface emailService;
	private final HelperInterface helperService;
	private final ActionInterface actionService;
	private UserDetails userDetails;

	@Override
  public void save(String name, String password, String email) throws SQLException, ClassNotFoundException {
    this.customerRepository.save(name, email, password);
  }

	@Override
	@Cacheable("customer")
	public Customer findOne(Integer id) throws SQLException, ClassNotFoundException {
		return this.customerRepository.findOne(id);
	}

	@Override
	@Cacheable("customer")
  public Customer findOne(String email) throws SQLException, ClassNotFoundException {
    return this.customerRepository.findOne(email);
  }

	@Override
  public void updateProfile(UpdateCustomerDto dto) throws SQLException, ClassNotFoundException {
		this.customerRepository.updateProfile(dto);
  }

	@Override
  public void updatePassword(String email, String pwd) throws ServerErrorException, SQLException, ClassNotFoundException {
		this.customerRepository.updatePassword(email, pwd);
  }

	@Override
  public void deleteCustomer(Integer idToDelete, Integer adminId) throws SQLException, ClassNotFoundException {
		String signedUserEmail = this.userDetails.getUsername();
		System.out.println(signedUserEmail);
		this.customerRepository.deleteCustomer(idToDelete, adminId);
  }

	@Override
	public void update2faStatus(ChangeTwoStepStatusDto dto) throws SQLException, ClassNotFoundException {
		this.customerRepository.update2faStatus(dto);
		ActionLog a = this.helperService.setActionLog(dto.email(),"Customer 2fa status was changed! Current status is: " + dto.status() + ".", dto.customerId());
		this.actionService.saveLog(a);
		this.emailService.sendTwoStepCode(dto.email());
	}

	@Override
	public List<Customer> findAll(Integer skip, Integer limit) throws SQLException, ClassNotFoundException {
		return this.customerRepository.findAll(skip, limit);
	}

	@Override
	public void updateBanStatus(Integer customerId, Boolean status) throws SQLException, ClassNotFoundException {
		this.customerRepository.updateBanStatus(customerId, status);
	}


	@Override
		public Boolean getTwoStepStatus(String email) throws SQLException, ClassNotFoundException {
			return this.customerRepository.getTwoStepStatus(email);
		}
}
