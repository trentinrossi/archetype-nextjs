import { Card, CardListItem, CreateCardRequest, UpdateCardRequest, PageResponse, ApiError } from '@/types/cardServices';

const API_BASE_URL = 'http://localhost:8080/api';

class CardService {
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

  async getCardByNumber(cardNumber: string): Promise<Card> {
    try {
      const response = await fetch(`${API_BASE_URL}/cards/${cardNumber}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<Card>(response);
    } catch (error) {
      console.error(`Error fetching card ${cardNumber}:`, error);
      throw error;
    }
  }

  async getCardsByAccountId(accountId: string): Promise<Card[]> {
    try {
      const response = await fetch(`${API_BASE_URL}/cards/account/${accountId}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<Card[]>(response);
    } catch (error) {
      console.error(`Error fetching cards by account ID ${accountId}:`, error);
      throw error;
    }
  }

  async getCardByAccountAndCardNumber(accountId: string, cardNumber: string): Promise<Card> {
    try {
      const response = await fetch(`${API_BASE_URL}/cards/account/${accountId}/card/${cardNumber}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<Card>(response);
    } catch (error) {
      console.error(`Error fetching card by account ${accountId} and card number ${cardNumber}:`, error);
      throw error;
    }
  }

  async getCardsList(params?: {
    accountId?: string;
    cardNumber?: string;
    page?: number;
    size?: number;
    sort?: string;
  }): Promise<PageResponse<CardListItem>> {
    try {
      const queryParams = new URLSearchParams();
      
      if (params?.accountId) {
        queryParams.append('accountId', params.accountId);
      }
      if (params?.cardNumber) {
        queryParams.append('cardNumber', params.cardNumber);
      }
      if (params?.page !== undefined) {
        queryParams.append('page', params.page.toString());
      }
      if (params?.size !== undefined) {
        queryParams.append('size', params.size.toString());
      }
      if (params?.sort) {
        queryParams.append('sort', params.sort);
      }

      const queryString = queryParams.toString();
      const url = queryString 
        ? `${API_BASE_URL}/cards/list?${queryString}`
        : `${API_BASE_URL}/cards/list`;

      const response = await fetch(url, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      
      return this.handleResponse<PageResponse<CardListItem>>(response);
    } catch (error) {
      console.error('Error fetching cards list:', error);
      throw error;
    }
  }

  async createCard(data: CreateCardRequest): Promise<Card> {
    try {
      const response = await fetch(`${API_BASE_URL}/cards`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(data),
      });
      return this.handleResponse<Card>(response);
    } catch (error) {
      console.error('Error creating card:', error);
      throw error;
    }
  }

  async updateCard(cardNumber: string, data: UpdateCardRequest): Promise<Card> {
    try {
      const response = await fetch(`${API_BASE_URL}/cards/${cardNumber}`, {
        method: 'PUT',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(data),
      });
      return this.handleResponse<Card>(response);
    } catch (error) {
      console.error(`Error updating card ${cardNumber}:`, error);
      throw error;
    }
  }

  async deleteCard(cardNumber: string): Promise<void> {
    try {
      const response = await fetch(`${API_BASE_URL}/cards/${cardNumber}`, {
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
      console.error(`Error deleting card ${cardNumber}:`, error);
      throw error;
    }
  }
}

export const cardService = new CardService();
export default CardService;
