import { Customer, CreateCustomerRequest, UpdateCustomerRequest, ApiError } from '@/types/cardServices';

const API_BASE_URL = '/api';

class CustomerService {
  private getAuthHeaders(): Record<string, string> {
    const token = typeof window !== 'undefined' ? localStorage.getItem('access_token') : null;
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  private async handleResponse<T>(response: Response): Promise<T> {
    if (!response.ok) {
      const errorData: ApiError = await response.json().catch(() => ({
        error: 'An unexpected error occurred',
        status: response.status,
      }));
      throw new Error(errorData.error || errorData.message || `HTTP error! status: ${response.status}`);
    }
    return response.json();
  }

  async getCustomers(): Promise<Customer[]> {
    try {
      const response = await fetch(`${API_BASE_URL}/customers`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<Customer[]>(response);
    } catch (error) {
      console.error('Error fetching customers:', error);
      throw error;
    }
  }

  async getCustomerById(customerId: string): Promise<Customer> {
    try {
      const response = await fetch(`${API_BASE_URL}/customers/${customerId}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<Customer>(response);
    } catch (error) {
      console.error(`Error fetching customer ${customerId}:`, error);
      throw error;
    }
  }

  async getCustomersByLastName(lastName: string): Promise<Customer[]> {
    try {
      const response = await fetch(`${API_BASE_URL}/customers/lastname/${encodeURIComponent(lastName)}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<Customer[]>(response);
    } catch (error) {
      console.error(`Error fetching customers by last name ${lastName}:`, error);
      throw error;
    }
  }

  async createCustomer(data: CreateCustomerRequest): Promise<Customer> {
    try {
      const response = await fetch(`${API_BASE_URL}/customers`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(data),
      });
      return this.handleResponse<Customer>(response);
    } catch (error) {
      console.error('Error creating customer:', error);
      throw error;
    }
  }

  async updateCustomer(customerId: string, data: UpdateCustomerRequest): Promise<Customer> {
    try {
      const response = await fetch(`${API_BASE_URL}/customers/${customerId}`, {
        method: 'PUT',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(data),
      });
      return this.handleResponse<Customer>(response);
    } catch (error) {
      console.error(`Error updating customer ${customerId}:`, error);
      throw error;
    }
  }

  async deleteCustomer(customerId: string): Promise<void> {
    try {
      const response = await fetch(`${API_BASE_URL}/customers/${customerId}`, {
        method: 'DELETE',
        headers: this.getAuthHeaders(),
      });
      if (!response.ok) {
        const errorData: ApiError = await response.json().catch(() => ({
          error: 'An unexpected error occurred',
          status: response.status,
        }));
        throw new Error(errorData.error || errorData.message || `HTTP error! status: ${response.status}`);
      }
    } catch (error) {
      console.error(`Error deleting customer ${customerId}:`, error);
      throw error;
    }
  }
}

export const customerService = new CustomerService();
export default CustomerService;
