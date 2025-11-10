'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { creditCardService } from '@/services/creditCardService';
import { CreditCard, UpdateCreditCardRequest } from '@/types/creditCard';
import { Input, Select, Button } from '@/components/ui';

export default function EditCreditCardPage() {
  const params = useParams();
  const router = useRouter();
  const [originalCard, setOriginalCard] = useState<CreditCard | null>(null);
  const [formData, setFormData] = useState<UpdateCreditCardRequest>({});
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [error, setError] = useState<string | null>(null);
  
  // Field-level validation errors
  const [errors, setErrors] = useState<Record<string, string>>({});

  useEffect(() => {
    if (params.cardNumber) {
      fetchCreditCard(params.cardNumber as string);
    }
  }, [params.cardNumber]);

  const fetchCreditCard = async (cardNumber: string) => {
    try {
      setLoading(true);
      const data = await creditCardService.getCreditCardByNumber(cardNumber);
      setOriginalCard(data);
      
      // Initialize form with current values
      setFormData({
        cardStatus: data.cardStatus,
        cardholderName: data.cardholderName || '',
        expiryMonth: data.expiryMonth || '',
        expiryYear: data.expiryYear || '',
        cardType: data.cardType || '',
        creditLimit: data.creditLimit,
        availableCredit: data.availableCredit,
      });
      
      setError(null);
    } catch (err) {
      setError('Failed to load credit card details');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const validateField = (name: string, value: any): string => {
    switch (name) {
      case 'cardStatus':
        if (value && !/^[A-Z]$/.test(value)) {
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
    
    // Validate all fields
    Object.keys(formData).forEach((key) => {
      const error = validateField(key, formData[key as keyof UpdateCreditCardRequest]);
      if (error) {
        newErrors[key] = error;
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
      setSaving(true);
      setError(null);
      
      // Only send fields that have changed
      const updates: UpdateCreditCardRequest = {};
      
      if (formData.cardStatus !== originalCard?.cardStatus) {
        updates.cardStatus = formData.cardStatus;
      }
      if (formData.cardholderName !== originalCard?.cardholderName) {
        updates.cardholderName = formData.cardholderName;
      }
      if (formData.expiryMonth !== originalCard?.expiryMonth) {
        updates.expiryMonth = formData.expiryMonth;
      }
      if (formData.expiryYear !== originalCard?.expiryYear) {
        updates.expiryYear = formData.expiryYear;
      }
      if (formData.cardType !== originalCard?.cardType) {
        updates.cardType = formData.cardType;
      }
      if (formData.creditLimit !== originalCard?.creditLimit) {
        updates.creditLimit = formData.creditLimit;
      }
      if (formData.availableCredit !== originalCard?.availableCredit) {
        updates.availableCredit = formData.availableCredit;
      }
      
      await creditCardService.updateCreditCard(params.cardNumber as string, updates);
      router.push(`/credit-cards/${params.cardNumber}`);
    } catch (err: any) {
      setError(err.message || 'Failed to update credit card');
      console.error(err);
    } finally {
      setSaving(false);
    }
  };

  const handleCancel = () => {
    router.push(`/credit-cards/${params.cardNumber}`);
  };

  if (loading) {
    return (
      <div className="p-6">
        <div className="flex items-center justify-center h-64">
          <div className="text-lg">Loading credit card details...</div>
        </div>
      </div>
    );
  }

  if (error && !originalCard) {
    return (
      <div className="p-6">
        <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded mb-4">
          {error}
        </div>
        <Button variant="secondary" onClick={() => router.push('/credit-cards')}>
          Back to List
        </Button>
      </div>
    );
  }

  if (!originalCard?.canModify) {
    return (
      <div className="p-6">
        <div className="bg-yellow-50 border border-yellow-200 text-yellow-700 px-4 py-3 rounded mb-4">
          This credit card cannot be modified (status: {originalCard?.cardStatusDisplayName})
        </div>
        <Button variant="secondary" onClick={() => router.push(`/credit-cards/${params.cardNumber}`)}>
          Back to Details
        </Button>
      </div>
    );
  }

  return (
    <div className="p-6 max-w-4xl">
      {/* Header */}
      <div className="mb-6">
        <h1 className="text-2xl font-bold">Edit Credit Card</h1>
        <p className="text-sm text-gray-600 mt-1">
          Card Number: {originalCard?.maskedCardNumber || params.cardNumber}
        </p>
      </div>

      {/* Error Message */}
      {error && (
        <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded mb-4">
          {error}
        </div>
      )}

      {/* Edit Form */}
      <form onSubmit={handleSubmit} className="bg-white shadow rounded-lg p-6">
        <div className="space-y-6">
          {/* Basic Information */}
          <div>
            <h2 className="text-lg font-semibold mb-4">Card Information</h2>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              <div>
                <Input
                  label="Cardholder Name"
                  value={formData.cardholderName || ''}
                  onChange={(e) => handleFieldChange('cardholderName', e.target.value)}
                  maxLength={100}
                />
                {errors.cardholderName && (
                  <div className="text-red-600 text-sm mt-1">{errors.cardholderName}</div>
                )}
              </div>
              
              <div>
                <Select
                  label="Card Status"
                  value={formData.cardStatus || ''}
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
                  onChange={(e) => handleFieldChange('creditLimit', e.target.value ? Number(e.target.value) : undefined)}
                  step="0.01"
                  min="0"
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
                  onChange={(e) => handleFieldChange('availableCredit', e.target.value ? Number(e.target.value) : undefined)}
                  step="0.01"
                  min="0"
                />
                {errors.availableCredit && (
                  <div className="text-red-600 text-sm mt-1">{errors.availableCredit}</div>
                )}
              </div>
            </div>
          </div>

          {/* Form Actions */}
          <div className="border-t border-gray-200 pt-6 flex gap-2">
            <Button type="submit" disabled={saving}>
              {saving ? 'Saving...' : 'Save Changes'}
            </Button>
            <Button
              type="button"
              variant="secondary"
              onClick={handleCancel}
              disabled={saving}
            >
              Cancel
            </Button>
          </div>
        </div>
      </form>
    </div>
  );
}
