import { 
  CreditCard, 
  CreateCreditCardRequest, 
  UpdateCreditCardRequest,
  PaginatedCreditCardResponse,
  CreditCardSearchFilters
} from '@/types/creditCard';

const API_BASE_URL = '/api';

class CreditCardService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async getCreditCards(page: number = 0, size: number = 20, sort?: string): Promise<PaginatedCreditCardResponse> {
    let url = `${API_BASE_URL}/credit-cards?page=${page}&size=${size}`;
    if (sort) {
      url += `&sort=${sort}`;
    }
    
    const response = await fetch(url, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    
    if (!response.ok) {
      throw new Error('Failed to fetch credit cards');
    }
    
    return response.json();
  }

  async getCreditCardsByAccount(
    accountId: string, 
    page: number = 0, 
    size: number = 20, 
    sort?: string
  ): Promise<PaginatedCreditCardResponse> {
    // Validate account ID format
    if (!/^\d{11}$/.test(accountId)) {
      throw new Error('Account ID must be exactly 11 digits');
    }
    
    let url = `${API_BASE_URL}/credit-cards/account/${accountId}?page=${page}&size=${size}`;
    if (sort) {
      url += `&sort=${sort}`;
    }
    
    const response = await fetch(url, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    
    if (!response.ok) {
      throw new Error('Failed to fetch credit cards');
    }
    
    return response.json();
  }

  async getCreditCardsByStatus(
    cardStatus: string,
    page: number = 0,
    size: number = 20,
    sort?: string
  ): Promise<PaginatedCreditCardResponse> {
    let url = `${API_BASE_URL}/credit-cards/status/${cardStatus}?page=${page}&size=${size}`;
    if (sort) {
      url += `&sort=${sort}`;
    }
    
    const response = await fetch(url, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    
    if (!response.ok) {
      throw new Error('Failed to fetch credit cards');
    }
    
    return response.json();
  }

  async searchCreditCards(
    filters: CreditCardSearchFilters,
    page: number = 0,
    size: number = 20,
    sort?: string
  ): Promise<PaginatedCreditCardResponse> {
    // If both accountId and cardNumberPattern are provided, use account-specific search
    if (filters.accountId && filters.cardNumberPattern) {
      // Validate account ID format
      if (!/^\d{11}$/.test(filters.accountId)) {
        throw new Error('Account ID must be exactly 11 digits');
      }
      
      let url = `${API_BASE_URL}/credit-cards/account/${filters.accountId}/search?cardNumberPattern=${filters.cardNumberPattern}&page=${page}&size=${size}`;
      if (sort) {
        url += `&sort=${sort}`;
      }
      
      const response = await fetch(url, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      
      if (!response.ok) {
        throw new Error('Failed to search credit cards');
      }
      
      return response.json();
    }
    
    // If only cardNumberPattern is provided
    if (filters.cardNumberPattern) {
      let url = `${API_BASE_URL}/credit-cards/search?cardNumberPattern=${filters.cardNumberPattern}&page=${page}&size=${size}`;
      if (sort) {
        url += `&sort=${sort}`;
      }
      
      const response = await fetch(url, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      
      if (!response.ok) {
        throw new Error('Failed to search credit cards');
      }
      
      return response.json();
    }
    
    // If only accountId is provided
    if (filters.accountId) {
      return this.getCreditCardsByAccount(filters.accountId, page, size, sort);
    }
    
    // If only status is provided
    if (filters.cardStatus) {
      return this.getCreditCardsByStatus(filters.cardStatus, page, size, sort);
    }
    
    // Default: get all cards
    return this.getCreditCards(page, size, sort);
  }

  async getCreditCardByNumber(cardNumber: string): Promise<CreditCard> {
    const response = await fetch(`${API_BASE_URL}/credit-cards/${cardNumber}`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    
    if (!response.ok) {
      throw new Error('Failed to fetch credit card');
    }
    
    return response.json();
  }

  async createCreditCard(data: CreateCreditCardRequest): Promise<CreditCard> {
    // Client-side validation
    if (!data.cardNumber || !/^\d{16}$/.test(data.cardNumber)) {
      throw new Error('Card number must be exactly 16 digits');
    }
    
    if (!data.accountId || !/^\d{11}$/.test(data.accountId)) {
      throw new Error('Account ID must be exactly 11 digits');
    }
    
    if (!data.cardStatus || !/^[A-Z]$/.test(data.cardStatus)) {
      throw new Error('Card status must be a single uppercase letter');
    }
    
    if (data.expiryMonth && !/^(0[1-9]|1[0-2])$/.test(data.expiryMonth)) {
      throw new Error('Expiry month must be between 01 and 12');
    }
    
    if (data.expiryYear && !/^\d{4}$/.test(data.expiryYear)) {
      throw new Error('Expiry year must be 4 digits');
    }
    
    const response = await fetch(`${API_BASE_URL}/credit-cards`, {
      method: 'POST',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    
    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(errorData.error || 'Failed to create credit card');
    }
    
    return response.json();
  }

  async updateCreditCard(cardNumber: string, data: UpdateCreditCardRequest): Promise<CreditCard> {
    // Client-side validation
    if (data.cardStatus && !/^[A-Z]$/.test(data.cardStatus)) {
      throw new Error('Card status must be a single uppercase letter');
    }
    
    if (data.expiryMonth && !/^(0[1-9]|1[0-2])$/.test(data.expiryMonth)) {
      throw new Error('Expiry month must be between 01 and 12');
    }
    
    if (data.expiryYear && !/^\d{4}$/.test(data.expiryYear)) {
      throw new Error('Expiry year must be 4 digits');
    }
    
    const response = await fetch(`${API_BASE_URL}/credit-cards/${cardNumber}`, {
      method: 'PUT',
      headers: this.getAuthHeaders(),
      body: JSON.stringify(data),
    });
    
    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(errorData.error || 'Failed to update credit card');
    }
    
    return response.json();
  }

  async deleteCreditCard(cardNumber: string): Promise<void> {
    const response = await fetch(`${API_BASE_URL}/credit-cards/${cardNumber}`, {
      method: 'DELETE',
      headers: this.getAuthHeaders(),
    });
    
    if (!response.ok) {
      throw new Error('Failed to delete credit card');
    }
  }

  async getCardCount(accountId: string): Promise<number> {
    const response = await fetch(`${API_BASE_URL}/credit-cards/account/${accountId}/count`, {
      method: 'GET',
      headers: this.getAuthHeaders(),
    });
    
    if (!response.ok) {
      throw new Error('Failed to get card count');
    }
    
    return response.json();
  }
}

export const creditCardService = new CreditCardService();
