import { CardCrossReference, CreateCardCrossReferenceRequest, ApiError } from '@/types/cardCrossReference';

const API_BASE_URL = 'http://localhost:8080/api';

class CardCrossReferenceService {
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
      throw new Error(errorData.error || `HTTP error! status: ${response.status}`);
    }
    return response.json();
  }

  async createCardCrossReference(data: CreateCardCrossReferenceRequest): Promise<CardCrossReference> {
    try {
      const response = await fetch(`${API_BASE_URL}/card-cross-references`, {
        method: 'POST',
        headers: this.getAuthHeaders(),
        body: JSON.stringify(data),
      });
      return this.handleResponse<CardCrossReference>(response);
    } catch (error) {
      console.error('Error creating card cross reference:', error);
      throw error;
    }
  }

  async getCardCrossReferenceByCardNumber(cardNumber: string): Promise<CardCrossReference> {
    try {
      const response = await fetch(`${API_BASE_URL}/card-cross-references/card/${cardNumber}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<CardCrossReference>(response);
    } catch (error) {
      console.error(`Error fetching card cross reference for card ${cardNumber}:`, error);
      throw error;
    }
  }

  async getCardCrossReferencesByAccountId(accountId: number): Promise<CardCrossReference[]> {
    try {
      const response = await fetch(`${API_BASE_URL}/card-cross-references/account/${accountId}`, {
        method: 'GET',
        headers: this.getAuthHeaders(),
      });
      return this.handleResponse<CardCrossReference[]>(response);
    } catch (error) {
      console.error(`Error fetching card cross references for account ${accountId}:`, error);
      throw error;
    }
  }

  async deleteCardCrossReference(cardNumber: string): Promise<void> {
    try {
      const response = await fetch(`${API_BASE_URL}/card-cross-references/${cardNumber}`, {
        method: 'DELETE',
        headers: this.getAuthHeaders(),
      });
      if (!response.ok) {
        const errorData: ApiError = await response.json().catch(() => ({
          error: 'An unexpected error occurred',
          status: response.status,
        }));
        throw new Error(errorData.error || `HTTP error! status: ${response.status}`);
      }
    } catch (error) {
      console.error(`Error deleting card cross reference ${cardNumber}:`, error);
      throw error;
    }
  }
}

export const cardCrossReferenceService = new CardCrossReferenceService();
export default CardCrossReferenceService;
