'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { Button, Input } from '@/components/ui';
import { transactionService } from '@/services/transactionService';

interface UpdateTransactionData {
  transactionDescription?: string;
  transactionAmount?: number;
  merchantName?: string;
  merchantCity?: string;
  merchantZip?: string;
}

const EditTransactionPage: React.FC = () => {
  const params = useParams();
  const router = useRouter();
  const transactionId = params.id as string;
  
  const [loading, setLoading] = useState(true);
  const [submitting, setSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const [formData, setFormData] = useState<UpdateTransactionData>({
    transactionDescription: '',
    transactionAmount: 0,
    merchantName: '',
    merchantCity: '',
    merchantZip: '',
  });

  const [originalData, setOriginalData] = useState({
    transactionId: '',
    cardNumber: '',
    accountId: '',
    transactionTypeCode: '',
    transactionCategoryCode: '',
    transactionSource: '',
    merchantId: 0,
    originalTimestamp: '',
    processingTimestamp: '',
  });

  const [fieldErrors, setFieldErrors] = useState<Record<string, string>>({});

  useEffect(() => {
    const fetchTransaction = async () => {
      try {
        setLoading(true);
        setError(null);
        const data = await transactionService.getTransactionById(transactionId);
        
        // Store non-editable fields
        setOriginalData({
          transactionId: data.transactionId,
          cardNumber: data.cardNumber,
          accountId: data.accountId,
          transactionTypeCode: data.transactionTypeCode,
          transactionCategoryCode: data.transactionCategoryCode,
          transactionSource: data.transactionSource,
          merchantId: data.merchantId,
          originalTimestamp: data.originalTimestamp,
          processingTimestamp: data.processingTimestamp,
        });
        
        // Populate form with editable data
        setFormData({
          transactionDescription: data.transactionDescription,
          transactionAmount: data.transactionAmount,
          merchantName: data.merchantName,
          merchantCity: data.merchantCity,
          merchantZip: data.merchantZip,
        });
      } catch (err) {
        console.error('Error fetching transaction:', err);
        setError(err instanceof Error ? err.message : 'Failed to fetch transaction');
      } finally {
        setLoading(false);
      }
    };

    if (transactionId) {
      fetchTransaction();
    }
  }, [transactionId]);

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement | HTMLTextAreaElement>) => {
    const { name, value } = e.target;
    
    // Clear field error when user starts typing
    if (fieldErrors[name]) {
      setFieldErrors(prev => {
        const newErrors = { ...prev };
        delete newErrors[name];
        return newErrors;
      });
    }

    // Handle number fields
    if (name === 'transactionAmount') {
      setFormData(prev => ({
        ...prev,
        [name]: value === '' ? 0 : parseFloat(value),
      }));
    } else {
      setFormData(prev => ({
        ...prev,
        [name]: value,
      }));
    }
  };

  const validateForm = (): boolean => {
    const errors: Record<string, string> = {};

    if (!formData.transactionDescription?.trim()) {
      errors.transactionDescription = 'Transaction description is required';
    }

    if (formData.transactionAmount === undefined || formData.transactionAmount === null) {
      errors.transactionAmount = 'Transaction amount is required';
    }

    if (!formData.merchantName?.trim()) {
      errors.merchantName = 'Merchant name is required';
    }

    setFieldErrors(errors);
    return Object.keys(errors).length === 0;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!validateForm()) {
      setError('Please fix the errors in the form');
      return;
    }

    try {
      setSubmitting(true);
      setError(null);

      // Note: You'll need to implement updateTransaction in the service
      // For now, this will show a success message and redirect
      // await transactionService.updateTransaction(transactionId, formData);
      
      // Temporary - remove this and uncomment above when API is ready
      await new Promise(resolve => setTimeout(resolve, 1000));
      console.log('Transaction update data:', formData);
      
      // Redirect to transaction details on success
      router.push(`/transactions/${transactionId}`);
    } catch (err) {
      console.error('Error updating transaction:', err);
      setError(err instanceof Error ? err.message : 'Failed to update transaction');
      setSubmitting(false);
    }
  };

  const handleCancel = () => {
    router.push(`/transactions/${transactionId}`);
  };

  const formatCardNumber = (cardNum: string): string => {
    if (!cardNum) return '';
    const cleaned = cardNum.replace(/\s/g, '');
    return cleaned.match(/.{1,4}/g)?.join(' ') || cardNum;
  };

  const getTransactionTypeLabel = (code: string): string => {
    const types: Record<string, string> = {
      'PURCHASE': 'Purchase',
      'PAYMENT': 'Payment',
      'REFUND': 'Refund',
      'CASH_ADVANCE': 'Cash Advance',
      'FEE': 'Fee',
      'INTEREST': 'Interest',
      'ADJUSTMENT': 'Adjustment',
    };
    return types[code] || code;
  };

  if (loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="inline-block animate-spin rounded-full h-12 w-12 border-b-2 border-gray-900"></div>
          <p className="mt-4 text-gray-600">Loading transaction...</p>
        </div>
      </div>
    );
  }

  return (
    <div className="container mx-auto px-4 py-8">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-gray-900 mb-2">Edit Transaction</h1>
        <p className="text-gray-600">Transaction ID: {originalData.transactionId}</p>
      </div>

      <div className="bg-white shadow-md rounded-lg overflow-hidden">
        <form onSubmit={handleSubmit} className="p-6">
          {error && (
            <div className="mb-6 bg-red-50 border border-red-200 text-red-800 px-4 py-3 rounded-md">
              <div className="flex">
                <div className="flex-shrink-0">
                  <svg className="h-5 w-5 text-red-400" viewBox="0 0 20 20" fill="currentColor">
                    <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z" clipRule="evenodd" />
                  </svg>
                </div>
                <div className="ml-3">
                  <p className="text-sm font-medium">{error}</p>
                </div>
              </div>
            </div>
          )}

          {/* Non-Editable Information */}
          <div className="mb-8 bg-gray-50 border border-gray-200 rounded-lg p-4">
            <h3 className="text-sm font-medium text-gray-700 mb-3">Transaction Information (Read-Only)</h3>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4 text-sm">
              <div>
                <span className="text-gray-500">Transaction ID:</span>
                <span className="ml-2 font-mono text-gray-900">{originalData.transactionId}</span>
              </div>
              <div>
                <span className="text-gray-500">Type:</span>
                <span className="ml-2 text-gray-900">{getTransactionTypeLabel(originalData.transactionTypeCode)}</span>
              </div>
              <div>
                <span className="text-gray-500">Card Number:</span>
                <span className="ml-2 font-mono text-gray-900">{formatCardNumber(originalData.cardNumber)}</span>
              </div>
              <div>
                <span className="text-gray-500">Account ID:</span>
                <span className="ml-2 text-gray-900">{originalData.accountId}</span>
              </div>
              <div>
                <span className="text-gray-500">Source:</span>
                <span className="ml-2 text-gray-900">{originalData.transactionSource}</span>
              </div>
              <div>
                <span className="text-gray-500">Merchant ID:</span>
                <span className="ml-2 text-gray-900">{originalData.merchantId}</span>
              </div>
            </div>
          </div>

          {/* Editable Transaction Details */}
          <div className="mb-8">
            <h2 className="text-xl font-semibold text-gray-900 mb-4 pb-2 border-b border-gray-200">
              Transaction Details
            </h2>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
              <div className="md:col-span-2">
                <label htmlFor="transactionDescription" className="block text-sm font-medium text-gray-700 mb-1">
                  Transaction Description <span className="text-red-500">*</span>
                </label>
                <textarea
                  id="transactionDescription"
                  name="transactionDescription"
                  value={formData.transactionDescription}
                  onChange={handleInputChange}
                  placeholder="Enter transaction description"
                  rows={3}
                  className={`flex w-full rounded-md border ${
                    fieldErrors.transactionDescription ? 'border-red-500' : 'border-gray-300'
                  } bg-white px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent disabled:cursor-not-allowed disabled:opacity-50`}
                  disabled={submitting}
                />
                {fieldErrors.transactionDescription && (
                  <p className="mt-1 text-sm text-red-600">{fieldErrors.transactionDescription}</p>
                )}
              </div>

              <div>
                <label htmlFor="transactionAmount" className="block text-sm font-medium text-gray-700 mb-1">
                  Transaction Amount <span className="text-red-500">*</span>
                </label>
                <Input
                  id="transactionAmount"
                  name="transactionAmount"
                  type="number"
                  step="0.01"
                  value={formData.transactionAmount}
                  onChange={handleInputChange}
                  placeholder="0.00"
                  className={fieldErrors.transactionAmount ? 'border-red-500' : ''}
                  disabled={submitting}
                />
                {fieldErrors.transactionAmount && (
                  <p className="mt-1 text-sm text-red-600">{fieldErrors.transactionAmount}</p>
                )}
                <p className="mt-1 text-xs text-gray-500">Use negative values for credits/refunds</p>
              </div>
            </div>
          </div>

          {/* Merchant Information */}
          <div className="mb-8">
            <h2 className="text-xl font-semibold text-gray-900 mb-4 pb-2 border-b border-gray-200">
              Merchant Information
            </h2>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
              <div className="md:col-span-2">
                <label htmlFor="merchantName" className="block text-sm font-medium text-gray-700 mb-1">
                  Merchant Name <span className="text-red-500">*</span>
                </label>
                <Input
                  id="merchantName"
                  name="merchantName"
                  type="text"
                  value={formData.merchantName}
                  onChange={handleInputChange}
                  placeholder="Enter merchant name"
                  className={fieldErrors.merchantName ? 'border-red-500' : ''}
                  disabled={submitting}
                />
                {fieldErrors.merchantName && (
                  <p className="mt-1 text-sm text-red-600">{fieldErrors.merchantName}</p>
                )}
              </div>

              <div>
                <label htmlFor="merchantCity" className="block text-sm font-medium text-gray-700 mb-1">
                  Merchant City
                </label>
                <Input
                  id="merchantCity"
                  name="merchantCity"
                  type="text"
                  value={formData.merchantCity}
                  onChange={handleInputChange}
                  placeholder="Enter city"
                  disabled={submitting}
                />
              </div>

              <div>
                <label htmlFor="merchantZip" className="block text-sm font-medium text-gray-700 mb-1">
                  Merchant ZIP Code
                </label>
                <Input
                  id="merchantZip"
                  name="merchantZip"
                  type="text"
                  value={formData.merchantZip}
                  onChange={handleInputChange}
                  placeholder="Enter ZIP code"
                  disabled={submitting}
                />
              </div>
            </div>
          </div>

          {/* Warning Boxes */}
          <div className="mt-6 bg-yellow-50 border border-yellow-200 rounded-md p-4 mb-6">
            <div className="flex">
              <div className="flex-shrink-0">
                <svg className="h-5 w-5 text-yellow-400" viewBox="0 0 20 20" fill="currentColor">
                  <path fillRule="evenodd" d="M8.257 3.099c.765-1.36 2.722-1.36 3.486 0l5.58 9.92c.75 1.334-.213 2.98-1.742 2.98H4.42c-1.53 0-2.493-1.646-1.743-2.98l5.58-9.92zM11 13a1 1 0 11-2 0 1 1 0 012 0zm-1-8a1 1 0 00-1 1v3a1 1 0 002 0V6a1 1 0 00-1-1z" clipRule="evenodd" />
                </svg>
              </div>
              <div className="ml-3">
                <h3 className="text-sm font-medium text-yellow-800">Important Notice</h3>
                <div className="mt-2 text-sm text-yellow-700">
                  <ul className="list-disc list-inside space-y-1">
                    <li>Transaction edits should only be made to correct data entry errors</li>
                    <li>All changes are logged and audited for compliance</li>
                    <li>Core transaction details (ID, card, account, type) cannot be modified</li>
                    <li>Contact your supervisor before making significant changes</li>
                  </ul>
                </div>
              </div>
            </div>
          </div>

          <div className="bg-blue-50 border border-blue-200 rounded-md p-4">
            <div className="flex">
              <div className="flex-shrink-0">
                <svg className="h-5 w-5 text-blue-400" viewBox="0 0 20 20" fill="currentColor">
                  <path fillRule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z" clipRule="evenodd" />
                </svg>
              </div>
              <div className="ml-3">
                <h3 className="text-sm font-medium text-blue-800">Restricted Fields</h3>
                <div className="mt-2 text-sm text-blue-700">
                  <p>Transaction ID, card number, account ID, transaction type, category, source, merchant ID, and timestamps are immutable for audit compliance.</p>
                </div>
              </div>
            </div>
          </div>

          {/* Form Actions */}
          <div className="mt-8 flex justify-end gap-3 pt-6 border-t border-gray-200">
            <Button
              type="button"
              variant="secondary"
              onClick={handleCancel}
              disabled={submitting}
            >
              Cancel
            </Button>
            <Button
              type="submit"
              disabled={submitting}
            >
              {submitting ? 'Updating...' : 'Update Transaction'}
            </Button>
          </div>
        </form>
      </div>
    </div>
  );
};

export default EditTransactionPage;
