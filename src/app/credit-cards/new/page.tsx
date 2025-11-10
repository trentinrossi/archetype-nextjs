'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { creditCardService } from '@/services/creditCardService';
import { CreateCreditCardRequest } from '@/types/creditCard';
import { Input, Select, Button } from '@/components/ui';

export default function CreateCreditCardPage() {
  const router = useRouter();
  const [formData, setFormData] = useState<CreateCreditCardRequest>({
    cardNumber: '',
    accountId: '',
    cardStatus: 'A',
    cardholderName: '',
    expiryMonth: '',
    expiryYear: '',
    cardType: '',
    creditLimit: 0,
    availableCredit: 0,
  });
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  
  // Field-level validation errors
  const [errors, setErrors] = useState<Record<string, string>>({});

  const validateField = (name: string, value: any): string => {
    switch (name) {
      case 'cardNumber':
        if (!value) {
          return 'Card number is required';
        }
        if (!/^\d{16}$/.test(value)) {
          return 'Card number must be exactly 16 digits';
        }
        break;
      case 'accountId':
        if (!value) {
          return 'Account ID is required';
        }
        if (!/^\d{11}$/.test(value)) {
          return 'Account ID must be exactly 11 digits';
        }
        break;
      case 'cardStatus':
        if (!value) {
          return 'Card status is required';
        }
        if (!/^[A-Z]$/.test(value)) {
          return 'Card status must be a single uppercase letter';
        }
        break;
      case 'expiryMonth':
        if (value && !/^(0[1-9]|1[0-2])$/.test(value)) {
          return 'Expiry month must be between 01 and 12';
        }
        break;
      case 'expiryYear':
        if (value && !/^\d{4}$/.test(value)) {
          return 'Expiry year must be 4 digits';
        }
        break;
      case 'creditLimit':
        if (value !== undefined && value < 0) {
          return 'Credit limit must be greater than or equal to 0';
        }
        break;
      case 'availableCredit':
        if (value !== undefined && value < 0) {
          return 'Available credit must be greater than or equal to 0';
        }
        if (formData.creditLimit !== undefined && value > formData.creditLimit) {
          return 'Available credit cannot exceed credit limit';
        }
        break;
    }
    return '';
  };

  const handleFieldChange = (name: string, value: any) => {
    setFormData({ ...formData, [name]: value });
    
    // Clear error for this field
    const newErrors = { ...errors };
    delete newErrors[name];
    setErrors(newErrors);
    
    // Validate field
    const error = validateField(name, value);
    if (error) {
      setErrors({ ...newErrors, [name]: error });
    }
  };

  const validateForm = (): boolean => {
    const newErrors: Record<string, string> = {};
    
    // Validate required fields
    const requiredFields = ['cardNumber', 'accountId', 'cardStatus'];
    requiredFields.forEach((field) => {
      const error = validateField(field, formData[field as keyof CreateCreditCardRequest]);
      if (error) {
        newErrors[field] = error;
      }
    });
    
    // Validate optional fields if provided
    Object.keys(formData).forEach((key) => {
      if (!requiredFields.includes(key)) {
        const value = formData[key as keyof CreateCreditCardRequest];
        if (value !== undefined && value !== '') {
          const error = validateField(key, value);
          if (error) {
            newErrors[key] = error;
          }
        }
      }
    });
    
    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!validateForm()) {
      setError('Please fix the validation errors before submitting');
      return;
    }
    
    try {
      setLoading(true);
      setError(null);
      
      // Prepare data - remove empty optional fields
      const dataToSubmit: CreateCreditCardRequest = {
        cardNumber: formData.cardNumber,
        accountId: formData.accountId,
        cardStatus: formData.cardStatus,
      };
      
      if (formData.cardholderName) {
        dataToSubmit.cardholderName = formData.cardholderName;
      }
      if (formData.expiryMonth) {
        dataToSubmit.expiryMonth = formData.expiryMonth;
      }
      if (formData.expiryYear) {
        dataToSubmit.expiryYear = formData.expiryYear;
      }
      if (formData.cardType) {
        dataToSubmit.cardType = formData.cardType;
      }
      if (formData.creditLimit !== undefined && formData.creditLimit > 0) {
        dataToSubmit.creditLimit = formData.creditLimit;
      }
      if (formData.availableCredit !== undefined && formData.availableCredit > 0) {
        dataToSubmit.availableCredit = formData.availableCredit;
      }
      
      await creditCardService.createCreditCard(dataToSubmit);
      router.push('/credit-cards');
    } catch (err: any) {
      setError(err.message || 'Failed to create credit card');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleCancel = () => {
    router.push('/credit-cards');
  };

  return (
    <div className="p-6 max-w-4xl">
      {/* Header */}
      <div className="mb-6">
        <h1 className="text-2xl font-bold">Create New Credit Card</h1>
        <p className="text-sm text-gray-600 mt-1">
          Enter the details for the new credit card
        </p>
      </div>

      {/* Error Message */}
      {error && (
        <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded mb-4">
          {error}
        </div>
      )}

      {/* Create Form */}
      <form onSubmit={handleSubmit} className="bg-white shadow rounded-lg p-6">
        <div className="space-y-6">
          {/* Required Information */}
          <div>
            <h2 className="text-lg font-semibold mb-4">Required Information</h2>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              <div>
                <Input
                  label="Card Number (16 digits)"
                  value={formData.cardNumber}
                  onChange={(e) => handleFieldChange('cardNumber', e.target.value)}
                  placeholder="1234567890123456"
                  maxLength={16}
                  required
                />
                {errors.cardNumber && (
                  <div className="text-red-600 text-sm mt-1">{errors.cardNumber}</div>
                )}
              </div>
              
              <div>
                <Input
                  label="Account ID (11 digits)"
                  value={formData.accountId}
                  onChange={(e) => handleFieldChange('accountId', e.target.value)}
                  placeholder="12345678901"
                  maxLength={11}
                  required
                />
                {errors.accountId && (
                  <div className="text-red-600 text-sm mt-1">{errors.accountId}</div>
                )}
              </div>
              
              <div>
                <Select
                  label="Card Status"
                  value={formData.cardStatus}
                  onChange={(e) => handleFieldChange('cardStatus', e.target.value)}
                  options={[
                    { value: 'A', label: 'Active' },
                    { value: 'I', label: 'Inactive' },
                    { value: 'B', label: 'Blocked' },
                    { value: 'C', label: 'Cancelled' },
                    { value: 'S', label: 'Suspended' },
                  ]}
                  required
                />
                {errors.cardStatus && (
                  <div className="text-red-600 text-sm mt-1">{errors.cardStatus}</div>
                )}
              </div>
            </div>
          </div>

          {/* Optional Information */}
          <div className="border-t border-gray-200 pt-6">
            <h2 className="text-lg font-semibold mb-4">Optional Information</h2>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              <div>
                <Input
                  label="Cardholder Name"
                  value={formData.cardholderName || ''}
                  onChange={(e) => handleFieldChange('cardholderName', e.target.value)}
                  placeholder="John Doe"
                  maxLength={100}
                />
                {errors.cardholderName && (
                  <div className="text-red-600 text-sm mt-1">{errors.cardholderName}</div>
                )}
              </div>
              
              <div>
                <Input
                  label="Card Type"
                  value={formData.cardType || ''}
                  onChange={(e) => handleFieldChange('cardType', e.target.value)}
                  placeholder="VISA, MASTERCARD, AMEX, etc."
                  maxLength={20}
                />
                {errors.cardType && (
                  <div className="text-red-600 text-sm mt-1">{errors.cardType}</div>
                )}
              </div>
            </div>
          </div>

          {/* Expiry Information */}
          <div className="border-t border-gray-200 pt-6">
            <h2 className="text-lg font-semibold mb-4">Expiry Information</h2>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              <div>
                <Input
                  label="Expiry Month (01-12)"
                  value={formData.expiryMonth || ''}
                  onChange={(e) => handleFieldChange('expiryMonth', e.target.value)}
                  placeholder="MM"
                  maxLength={2}
                />
                {errors.expiryMonth && (
                  <div className="text-red-600 text-sm mt-1">{errors.expiryMonth}</div>
                )}
              </div>
              
              <div>
                <Input
                  label="Expiry Year (YYYY)"
                  value={formData.expiryYear || ''}
                  onChange={(e) => handleFieldChange('expiryYear', e.target.value)}
                  placeholder="YYYY"
                  maxLength={4}
                />
                {errors.expiryYear && (
                  <div className="text-red-600 text-sm mt-1">{errors.expiryYear}</div>
                )}
              </div>
            </div>
          </div>

          {/* Financial Information */}
          <div className="border-t border-gray-200 pt-6">
            <h2 className="text-lg font-semibold mb-4">Financial Information</h2>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              <div>
                <Input
                  label="Credit Limit"
                  type="number"
                  value={formData.creditLimit !== undefined ? formData.creditLimit : ''}
                  onChange={(e) => handleFieldChange('creditLimit', e.target.value ? Number(e.target.value) : 0)}
                  step="0.01"
                  min="0"
                  placeholder="5000.00"
                />
                {errors.creditLimit && (
                  <div className="text-red-600 text-sm mt-1">{errors.creditLimit}</div>
                )}
              </div>
              
              <div>
                <Input
                  label="Available Credit"
                  type="number"
                  value={formData.availableCredit !== undefined ? formData.availableCredit : ''}
                  onChange={(e) => handleFieldChange('availableCredit', e.target.value ? Number(e.target.value) : 0)}
                  step="0.01"
                  min="0"
                  placeholder="5000.00"
                />
                {errors.availableCredit && (
                  <div className="text-red-600 text-sm mt-1">{errors.availableCredit}</div>
                )}
                <p className="text-xs text-gray-500 mt-1">
                  Typically set equal to credit limit for new cards
                </p>
              </div>
            </div>
          </div>

          {/* Form Actions */}
          <div className="border-t border-gray-200 pt-6 flex gap-2">
            <Button type="submit" disabled={loading}>
              {loading ? 'Creating...' : 'Create Credit Card'}
            </Button>
            <Button
              type="button"
              variant="secondary"
              onClick={handleCancel}
              disabled={loading}
            >
              Cancel
            </Button>
          </div>
        </div>
      </form>
    </div>
  );
}
