'use client';

import React, { useState } from 'react';
import { Input, Button } from '@/components/ui';
import { CreateAccountRequest, Account } from '@/types/account';
import { accountService } from '@/services/accountService';

interface AccountFormData {
  accountId: string;
  currentBalance: string;
  creditLimit: string;
  currentCycleCredit: string;
  currentCycleDebit: string;
  expirationDate: string;
}

interface AccountFormProps {
  onSuccess?: (account: Account) => void;
  onCancel?: () => void;
}

interface FormErrors {
  [key: string]: string;
}

const AccountForm: React.FC<AccountFormProps> = ({ onSuccess, onCancel }) => {
  const [formData, setFormData] = useState<AccountFormData>({
    accountId: '',
    currentBalance: '0.00',
    creditLimit: '',
    currentCycleCredit: '0.00',
    currentCycleDebit: '0.00',
    expirationDate: '',
  });

  const [errors, setErrors] = useState<FormErrors>({});
  const [loading, setLoading] = useState(false);
  const [apiError, setApiError] = useState<string | null>(null);

  const validateForm = (): boolean => {
    const newErrors: FormErrors = {};

    if (!formData.accountId) {
      newErrors.accountId = 'Account ID is required';
    } else if (!/^\d{1,11}$/.test(formData.accountId)) {
      newErrors.accountId = 'Account ID must be numeric (max 11 digits)';
    }

    if (!formData.creditLimit) {
      newErrors.creditLimit = 'Credit limit is required';
    } else if (isNaN(parseFloat(formData.creditLimit)) || parseFloat(formData.creditLimit) <= 0) {
      newErrors.creditLimit = 'Credit limit must be a valid positive number';
    }

    if (!formData.currentBalance) {
      newErrors.currentBalance = 'Current balance is required';
    } else if (isNaN(parseFloat(formData.currentBalance))) {
      newErrors.currentBalance = 'Current balance must be a valid number';
    }

    if (!formData.currentCycleCredit) {
      newErrors.currentCycleCredit = 'Current cycle credit is required';
    } else if (isNaN(parseFloat(formData.currentCycleCredit))) {
      newErrors.currentCycleCredit = 'Current cycle credit must be a valid number';
    }

    if (!formData.currentCycleDebit) {
      newErrors.currentCycleDebit = 'Current cycle debit is required';
    } else if (isNaN(parseFloat(formData.currentCycleDebit))) {
      newErrors.currentCycleDebit = 'Current cycle debit must be a valid number';
    }

    if (!formData.expirationDate) {
      newErrors.expirationDate = 'Expiration date is required';
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleInputChange = (field: keyof AccountFormData, value: string) => {
    setFormData((prev) => ({
      ...prev,
      [field]: value,
    }));

    if (errors[field]) {
      setErrors((prev) => {
        const newErrors = { ...prev };
        delete newErrors[field];
        return newErrors;
      });
    }

    setApiError(null);
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    if (!validateForm()) {
      return;
    }

    try {
      setLoading(true);
      setApiError(null);

      const payload: CreateAccountRequest = {
        accountId: parseInt(formData.accountId),
        currentBalance: parseFloat(formData.currentBalance),
        creditLimit: parseFloat(formData.creditLimit),
        currentCycleCredit: parseFloat(formData.currentCycleCredit),
        currentCycleDebit: parseFloat(formData.currentCycleDebit),
        expirationDate: formData.expirationDate,
      };

      const account = await accountService.createAccount(payload);

      if (onSuccess) {
        onSuccess(account);
      }

      // Reset form
      setFormData({
        accountId: '',
        currentBalance: '0.00',
        creditLimit: '',
        currentCycleCredit: '0.00',
        currentCycleDebit: '0.00',
        expirationDate: '',
      });
    } catch (err) {
      console.error('Error creating account:', err);
      const errorMessage = err instanceof Error ? err.message : 'An unexpected error occurred';
      setApiError(errorMessage);
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="bg-white shadow rounded-lg p-6">
      <h2 className="text-2xl font-bold mb-6">Create New Account</h2>

      {apiError && (
        <div className="mb-6 bg-red-50 border border-red-200 rounded-lg p-4">
          <div className="flex">
            <div className="flex-shrink-0">
              <svg className="h-5 w-5 text-red-400" viewBox="0 0 20 20" fill="currentColor">
                <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z" clipRule="evenodd" />
              </svg>
            </div>
            <div className="ml-3">
              <h3 className="text-sm font-medium text-red-800">Error</h3>
              <p className="mt-1 text-sm text-red-700">{apiError}</p>
            </div>
          </div>
        </div>
      )}

      <form onSubmit={handleSubmit} className="space-y-6">
        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          <Input
            label="Account ID"
            placeholder="Enter account ID (max 11 digits)"
            value={formData.accountId}
            onChange={(e) => handleInputChange('accountId', e.target.value)}
            error={errors.accountId}
            required
          />

          <Input
            label="Credit Limit"
            type="number"
            step="0.01"
            placeholder="Enter credit limit"
            value={formData.creditLimit}
            onChange={(e) => handleInputChange('creditLimit', e.target.value)}
            error={errors.creditLimit}
            required
          />

          <Input
            label="Current Balance"
            type="number"
            step="0.01"
            placeholder="Enter current balance"
            value={formData.currentBalance}
            onChange={(e) => handleInputChange('currentBalance', e.target.value)}
            error={errors.currentBalance}
            required
          />

          <Input
            label="Current Cycle Credit"
            type="number"
            step="0.01"
            placeholder="Enter current cycle credit"
            value={formData.currentCycleCredit}
            onChange={(e) => handleInputChange('currentCycleCredit', e.target.value)}
            error={errors.currentCycleCredit}
            required
          />

          <Input
            label="Current Cycle Debit"
            type="number"
            step="0.01"
            placeholder="Enter current cycle debit"
            value={formData.currentCycleDebit}
            onChange={(e) => handleInputChange('currentCycleDebit', e.target.value)}
            error={errors.currentCycleDebit}
            required
          />

          <Input
            label="Expiration Date"
            type="date"
            value={formData.expirationDate}
            onChange={(e) => handleInputChange('expirationDate', e.target.value)}
            error={errors.expirationDate}
            required
          />
        </div>

        <div className="flex gap-4 pt-4">
          <Button type="submit" disabled={loading}>
            {loading ? 'Creating Account...' : 'Create Account'}
          </Button>
          {onCancel && (
            <Button type="button" variant="secondary" onClick={onCancel} disabled={loading}>
              Cancel
            </Button>
          )}
        </div>
      </form>
    </div>
  );
};

export default AccountForm;
