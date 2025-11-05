'use client';

import React, { useState } from 'react';
import { Input, Button } from '@/components/ui';
import { CreateTransactionRequest, Transaction } from '@/types/transaction';
import { transactionService } from '@/services/transactionService';

interface TransactionFormData {
  cardNumber: string;
  typeCode: string;
  categoryCode: string;
  source: string;
  description: string;
  amount: string;
  merchantId: string;
  merchantName: string;
  merchantCity: string;
  merchantZip: string;
  originalTimestamp: string;
  processingTimestamp: string;
}

interface TransactionFormProps {
  onSuccess?: (transaction: Transaction) => void;
  onCancel?: () => void;
}

interface FormErrors {
  [key: string]: string;
}

const TransactionForm: React.FC<TransactionFormProps> = ({ onSuccess, onCancel }) => {
  const [formData, setFormData] = useState<TransactionFormData>({
    cardNumber: '',
    typeCode: '',
    categoryCode: '',
    source: '',
    description: '',
    amount: '',
    merchantId: '',
    merchantName: '',
    merchantCity: '',
    merchantZip: '',
    originalTimestamp: '',
    processingTimestamp: '',
  });

  const [errors, setErrors] = useState<FormErrors>({});
  const [loading, setLoading] = useState(false);
  const [apiError, setApiError] = useState<string | null>(null);

  const validateForm = (): boolean => {
    const newErrors: FormErrors = {};

    if (!formData.cardNumber) {
      newErrors.cardNumber = 'Card number is required';
    } else if (!/^\d{16}$/.test(formData.cardNumber)) {
      newErrors.cardNumber = 'Card number must be exactly 16 digits';
    }

    if (!formData.typeCode) {
      newErrors.typeCode = 'Type code is required';
    } else if (!/^\d{2}$/.test(formData.typeCode)) {
      newErrors.typeCode = 'Type code must be exactly 2 digits';
    }

    if (!formData.categoryCode) {
      newErrors.categoryCode = 'Category code is required';
    } else if (!/^\d{4}$/.test(formData.categoryCode)) {
      newErrors.categoryCode = 'Category code must be exactly 4 digits';
    }

    if (!formData.source) {
      newErrors.source = 'Source is required';
    } else if (formData.source.length > 10) {
      newErrors.source = 'Source must not exceed 10 characters';
    }

    if (formData.description && formData.description.length > 100) {
      newErrors.description = 'Description must not exceed 100 characters';
    }

    if (!formData.amount) {
      newErrors.amount = 'Amount is required';
    } else if (isNaN(parseFloat(formData.amount)) || parseFloat(formData.amount) <= 0) {
      newErrors.amount = 'Amount must be a valid positive number';
    }

    if (!formData.merchantId) {
      newErrors.merchantId = 'Merchant ID is required';
    } else if (!/^\d{1,9}$/.test(formData.merchantId)) {
      newErrors.merchantId = 'Merchant ID must be numeric (max 9 digits)';
    }

    if (!formData.merchantName) {
      newErrors.merchantName = 'Merchant name is required';
    } else if (formData.merchantName.length > 50) {
      newErrors.merchantName = 'Merchant name must not exceed 50 characters';
    }

    if (!formData.originalTimestamp) {
      newErrors.originalTimestamp = 'Original timestamp is required';
    }

    if (!formData.processingTimestamp) {
      newErrors.processingTimestamp = 'Processing timestamp is required';
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleInputChange = (field: keyof TransactionFormData, value: string) => {
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

      const payload: CreateTransactionRequest = {
        cardNumber: formData.cardNumber,
        typeCode: formData.typeCode,
        categoryCode: formData.categoryCode,
        source: formData.source,
        description: formData.description || undefined,
        amount: parseFloat(formData.amount),
        merchantId: parseInt(formData.merchantId),
        merchantName: formData.merchantName,
        merchantCity: formData.merchantCity || undefined,
        merchantZip: formData.merchantZip || undefined,
        originalTimestamp: new Date(formData.originalTimestamp).toISOString(),
        processingTimestamp: new Date(formData.processingTimestamp).toISOString(),
      };

      const transaction = await transactionService.createTransaction(payload);

      if (onSuccess) {
        onSuccess(transaction);
      }

      // Reset form
      setFormData({
        cardNumber: '',
        typeCode: '',
        categoryCode: '',
        source: '',
        description: '',
        amount: '',
        merchantId: '',
        merchantName: '',
        merchantCity: '',
        merchantZip: '',
        originalTimestamp: '',
        processingTimestamp: '',
      });
    } catch (err) {
      console.error('Error creating transaction:', err);
      const errorMessage = err instanceof Error ? err.message : 'An unexpected error occurred';
      
      // Parse error codes from message
      if (errorMessage.includes('100') || errorMessage.toLowerCase().includes('invalid card')) {
        setApiError('Invalid card number. Please verify the card number is correct.');
      } else if (errorMessage.includes('101') || errorMessage.toLowerCase().includes('account not found')) {
        setApiError('Account not found. The card is not associated with any account.');
      } else if (errorMessage.includes('102') || errorMessage.toLowerCase().includes('overlimit')) {
        setApiError('Transaction exceeds account limit. Please contact support.');
      } else if (errorMessage.includes('103') || errorMessage.toLowerCase().includes('expiration')) {
        setApiError('Account has expired. Please contact support.');
      } else {
        setApiError(errorMessage);
      }
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="bg-white shadow rounded-lg p-6">
      <h2 className="text-2xl font-bold mb-6">Create New Transaction</h2>

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
            label="Card Number"
            placeholder="Enter 16-digit card number"
            value={formData.cardNumber}
            onChange={(e) => handleInputChange('cardNumber', e.target.value)}
            error={errors.cardNumber}
            required
          />

          <Input
            label="Type Code"
            placeholder="Enter 2-digit type code"
            value={formData.typeCode}
            onChange={(e) => handleInputChange('typeCode', e.target.value)}
            error={errors.typeCode}
            required
          />

          <Input
            label="Category Code"
            placeholder="Enter 4-digit category code"
            value={formData.categoryCode}
            onChange={(e) => handleInputChange('categoryCode', e.target.value)}
            error={errors.categoryCode}
            required
          />

          <Input
            label="Source"
            placeholder="Enter source (max 10 chars)"
            value={formData.source}
            onChange={(e) => handleInputChange('source', e.target.value)}
            error={errors.source}
            required
          />

          <Input
            label="Amount"
            type="number"
            step="0.01"
            placeholder="Enter amount"
            value={formData.amount}
            onChange={(e) => handleInputChange('amount', e.target.value)}
            error={errors.amount}
            required
          />

          <Input
            label="Merchant ID"
            placeholder="Enter merchant ID"
            value={formData.merchantId}
            onChange={(e) => handleInputChange('merchantId', e.target.value)}
            error={errors.merchantId}
            required
          />

          <Input
            label="Merchant Name"
            placeholder="Enter merchant name (max 50 chars)"
            value={formData.merchantName}
            onChange={(e) => handleInputChange('merchantName', e.target.value)}
            error={errors.merchantName}
            required
          />

          <Input
            label="Merchant City"
            placeholder="Enter merchant city (optional)"
            value={formData.merchantCity}
            onChange={(e) => handleInputChange('merchantCity', e.target.value)}
            error={errors.merchantCity}
          />

          <Input
            label="Merchant ZIP"
            placeholder="Enter merchant ZIP (optional)"
            value={formData.merchantZip}
            onChange={(e) => handleInputChange('merchantZip', e.target.value)}
            error={errors.merchantZip}
          />

          <Input
            label="Original Timestamp"
            type="datetime-local"
            value={formData.originalTimestamp}
            onChange={(e) => handleInputChange('originalTimestamp', e.target.value)}
            error={errors.originalTimestamp}
            required
          />

          <Input
            label="Processing Timestamp"
            type="datetime-local"
            value={formData.processingTimestamp}
            onChange={(e) => handleInputChange('processingTimestamp', e.target.value)}
            error={errors.processingTimestamp}
            required
          />
        </div>

        <div className="col-span-2">
          <Input
            label="Description"
            placeholder="Enter description (optional, max 100 chars)"
            value={formData.description}
            onChange={(e) => handleInputChange('description', e.target.value)}
            error={errors.description}
          />
        </div>

        <div className="flex gap-4 pt-4">
          <Button type="submit" disabled={loading}>
            {loading ? 'Creating Transaction...' : 'Create Transaction'}
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

export default TransactionForm;
