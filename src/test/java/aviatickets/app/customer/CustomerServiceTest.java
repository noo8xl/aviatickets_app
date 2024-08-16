package aviatickets.app.customer;

import aviatickets.app.customer.dto.ChangeTwoStepStatusDto;
import aviatickets.app.customer.dto.UpdateCustomerDto;
import aviatickets.app.customer.entity.Customer;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.when;

@SpringBootTest(classes = CustomerService.class)
class CustomerServiceTest {

	private final Logger log = LoggerFactory.getLogger(CustomerServiceTest.class);

	private Customer customer = new Customer();
	private Customer testUser = new Customer();
	private List<Customer> userList = new ArrayList<>();
	private Boolean twoStepStatus = false;

	private UpdateCustomerDto updateCustomerDto;
	private ChangeTwoStepStatusDto changeTwoStepStatusDto;

	@MockBean
	CustomerInterface repo;

	@BeforeEach
	void setUp() {
		this.customer.setCustomer(
			1,
			"user1",
			"email@example.com",
			"qwerty123456",
			false,
			false
		);

		this.testUser.setCustomer(
			2,
			"user2",
			"mail@ex.com",
			"pwd123qwerty",
			false,
			false
		);

		this.userList.add(this.customer);
		this.userList.add(this.testUser);

		this.updateCustomerDto = new UpdateCustomerDto(
			1,
			"user1",
			"email@example.com",
			"qwerty123456"
		);

		this.changeTwoStepStatusDto = new ChangeTwoStepStatusDto(
			1,
			"email@example.com",
			true
		);
	}

	@AfterEach
	void tearDown() {
		this.customer	= null;
		this.testUser = null;
		this.userList = null;
		this.twoStepStatus = null;
		this.updateCustomerDto = null;
		this.changeTwoStepStatusDto = null;
		this.repo = null;
	}

	@Test
	void save() throws SQLException, ClassNotFoundException {

		this.repo.save(
				this.customer.getName(),
				this.customer.getPassword(),
				this.customer.getEmail()
		);

		Mockito.verify(this.repo, times(1)).save(
				this.customer.getName(),
				this.customer.getPassword(),
				this.customer.getEmail()
		);
		log.info("saveCustomer test is OK");
	}

	@Test
	void findOneById() throws SQLException, ClassNotFoundException {
		when(this.repo.findOne(2)).thenReturn(this.testUser);

		Customer foundUser = this.repo.findOne(2);
		Customer c = foundUser.getCustomer();

		assertNotNull(c);
		assertEquals(c.getName(), "user2");
		assertEquals(c.getUsername(), "mail@ex.com");

		log.info("findOne by id test is OK");
	}


	@Test
	void findOneByEmail() throws SQLException, ClassNotFoundException {
		when(this.repo.findOne("email@example.com")).thenReturn(this.customer);

		Customer foundUser = this.repo.findOne("email@example.com");
		Customer c = foundUser.getCustomer();

		assertNotNull(c);
		assertEquals(c.getName(), "user1");
		assertEquals(c.getUsername(), "email@example.com");

		log.info("findOne by email test is OK");
	}

	@Test
	void updateProfile() throws SQLException, ClassNotFoundException {
		this.repo.updateProfile(this.updateCustomerDto);

		Mockito
			.verify(this.repo, times(1))
			.updateProfile(this.updateCustomerDto);

		log.info("updateProfile test is OK");
	}

	@Test
	void updatePassword() throws SQLException, ClassNotFoundException {
		this.repo.updatePassword(this.customer.getEmail(), "updatedPWD<test>");

		Mockito
			.verify(this.repo, times(1))
			.updatePassword(this.customer.getEmail(), "updatedPWD<test>");

		log.info("updatePassword test is OK");
	}

	@Test
	void deleteCustomer() throws SQLException, ClassNotFoundException {
		this.repo.deleteCustomer(2, 1);

		Mockito
				.verify(this.repo, times(1))
				.deleteCustomer(2,1);

		log.info("deleteCustomer test is OK");
	}

	@Test
	void update2faStatus() throws SQLException, ClassNotFoundException {
		this.repo.update2faStatus(this.changeTwoStepStatusDto);

		Mockito
				.verify(this.repo, times(1))
				.update2faStatus(this.changeTwoStepStatusDto);

		log.info("update2faStatus test is OK");
	}

	@Test
	void findAll() throws SQLException, ClassNotFoundException {
		when(this.repo.findAll(0,10)).thenReturn(this.userList);

		List<Customer> cList = this.repo.findAll(0,10);
		Customer c = cList.getFirst().getCustomer();

		assertNotNull(c);
		assertEquals(cList.size(), 2);
		assertEquals(c.getName(), "user1");
		assertEquals(c.getUsername(), "email@example.com");

		log.info("findAll test is OK");
	}

	@Test
	void updateBanStatus() throws SQLException, ClassNotFoundException {
		this.repo.updateBanStatus(2, true);

		Mockito
				.verify(this.repo, times(1))
				.updateBanStatus(2, true);

		log.info("updateBanStatus test is OK");
	}

	@Test
	void getTwoStepStatus() throws SQLException, ClassNotFoundException {
		when(this.repo.getTwoStepStatus(this.customer.getEmail())).thenReturn(this.twoStepStatus);

		Boolean status = this.repo.getTwoStepStatus(this.customer.getEmail());

		assertNotNull(status);
		log.info("getTwoStepStatus test is OK");
	}
}