import { InterestCalculation } from '@/types/cardDemo';

const API_BASE_URL = '/api';

class InterestService {
  private getAuthHeaders(): Record<string, string> {
    const token = localStorage.getItem('access_token');
    return {
      'Content-Type': 'application/json',
      ...(token && { Authorization: `Bearer ${token}` }),
    };
  }

  async calculateInterest(processingDate: string): Promise<InterestCalculation[]> {
    const response = await fetch(
      `${API_BASE_URL}/interest/calculate?processingDate=${encodeURIComponent(processingDate)}`,
      {
        method: 'POST',
        headers: this.getAuthHeaders(),
      }
    );
    if (!response.ok) {
      throw new Error('Failed to calculate interest');
    }
    return response.json();
  }
}

export const interestService = new InterestService();
