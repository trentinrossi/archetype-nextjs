'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { cardService } from '@/services/cardService';
import { Card, UpdateCardRequest } from '@/types/card';
import { Input, Select, Button } from '@/components/ui';

/**
 * Card Update Screen (COCRDUPC)
 * 
 * Purpose: Update existing credit card information
 * 
 * Features:
 * - Edit embossed name (alphabets and spaces only)
 * - Edit expiration date (must be in future)
 * - Edit active status (Y/N)
 * - Full validation as per business rules
 * - Return to detail view or list
 */
export default function EditCardPage() {
  const params = useParams();
  const router = useRouter();
  const [formData, setFormData] = useState<UpdateCardRequest>({});
  const [originalCard, setOriginalCard] = useState<Card | null>(null);
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [validationErrors, setValidationErrors] = useState<Record<string, string>>({});

  useEffect(() => {
    if (params.cardNumber) {
      fetchCard(params.cardNumber as string);
    }
  }, [params.cardNumber]);

  const fetchCard = async (cardNumber: string) => {
    try {
      setLoading(true);
      const data = await cardService.getCardByNumber(cardNumber);
      setOriginalCard(data);
      setFormData({
        embossedName: data.embossedName,
        expirationDate: data.expirationDate.split('T')[0], // Format for date input
        activeStatus: data.activeStatus,
      });
      setError(null);
    } catch (err) {
      setError('Failed to load card details');
      console.error('Failed to load card:', err);
    } finally {
      setLoading(false);
    }
  };

  /**
   * Validate embossed name (alphabets and spaces only)
   */
  const validateEmbossedName = (value: string): boolean => {
    if (!value || value.trim().length === 0) {
      setValidationErrors((prev) => ({
        ...prev,
        embossedName: 'Embossed name is required',
      }));
      return false;
    }
    
    if (!/^[A-Za-z\s]+$/.test(value)) {
      setValidationErrors((prev) => ({
        ...prev,
        embossedName: 'Embossed name can only contain alphabets and spaces',
      }));
      return false;
    }
    
    setValidationErrors((prev) => {
      const { embossedName, ...rest } = prev;
      return rest;
    });
    return true;
  };

  /**
   * Validate expiration date (must be in future)
   */
  const validateExpirationDate = (value: string): boolean => {
    if (!value) {
      setValidationErrors((prev) => ({
        ...prev,
        expirationDate: 'Expiration date is required',
      }));
      return false;
    }
    
    const expirationDate = new Date(value);
    const today = new Date();
    today.setHours(0, 0, 0, 0);
    
    if (expirationDate <= today) {
      setValidationErrors((prev) => ({
        ...prev,
        expirationDate: 'Expiration date must be in the future',
      }));
      return false;
    }
    
    setValidationErrors((prev) => {
      const { expirationDate, ...rest } = prev;
      return rest;
    });
    return true;
  };

  /**
   * Validate active status (must be Y or N)
   */
  const validateActiveStatus = (value: string): boolean => {
    if (!value || (value !== 'Y' && value !== 'N')) {
      setValidationErrors((prev) => ({
        ...prev,
        activeStatus: 'Active status must be Y or N',
      }));
      return false;
    }
    
    setValidationErrors((prev) => {
      const { activeStatus, ...rest } = prev;
      return rest;
    });
    return true;
  };

  /**
   * Validate all fields
   */
  const validateForm = (): boolean => {
    const isNameValid = validateEmbossedName(formData.embossedName || '');
    const isDateValid = validateExpirationDate(formData.expirationDate || '');
    const isStatusValid = validateActiveStatus(formData.activeStatus || '');
    
    return isNameValid && isDateValid && isStatusValid;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setError(null);

    // Validate form
    if (!validateForm()) {
      setError('Please correct the validation errors before submitting');
      return;
    }

    try {
      setSaving(true);
      await cardService.updateCard(params.cardNumber as string, formData);
      router.push(`/cards/${params.cardNumber}`);
    } catch (err) {
      setError('Failed to update card');
      console.error(err);
    } finally {
      setSaving(false);
    }
  };

  const handleCancel = () => {
    router.push(`/cards/${params.cardNumber}`);
  };

  const handleBackToList = () => {
    router.push('/cards');
  };

  if (loading) {
    return (
      <div className="p-6 flex items-center justify-center min-h-screen">
        <div className="text-lg">Loading card details...</div>
      </div>
    );
  }

  if (error && !originalCard) {
    return (
      <div className="p-6">
        <div className="bg-red-100 text-red-800 p-4 rounded-lg mb-6">
          {error}
        </div>
        <Button variant="secondary" onClick={handleBackToList}>
          Back to Card List
        </Button>
      </div>
    );
  }

  return (
    <div className="p-6 max-w-4xl mx-auto">
      {/* Header */}
      <div className="bg-blue-900 text-white p-4 mb-6 rounded-lg">
        <div className="flex justify-between items-center">
          <div>
            <h1 className="text-2xl font-bold">UPDATE CARD</h1>
            <p className="text-sm">Transaction: COCRDUPC</p>
          </div>
        </div>
      </div>

      {/* Page Title */}
      <div className="mb-6">
        <h2 className="text-xl font-bold">Edit Card Information</h2>
        <p className="text-sm text-gray-600 mt-1">
          Card Number: <span className="font-mono">{params.cardNumber}</span>
        </p>
      </div>

      {/* Error Message */}
      {error && (
        <div className="bg-red-100 text-red-800 p-4 rounded-lg mb-6">
          {error}
        </div>
      )}

      {/* Edit Form */}
      <form onSubmit={handleSubmit} className="bg-white shadow rounded-lg p-6">
        <div className="space-y-6">
          {/* Card Number (Read-only) */}
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-2">
              Card Number
            </label>
            <div className="px-4 py-2 bg-gray-100 rounded border border-gray-300 font-mono">
              {params.cardNumber}
            </div>
            <p className="text-xs text-gray-500 mt-1">Card number cannot be changed</p>
          </div>

          {/* Account ID (Read-only) */}
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-2">
              Account ID
            </label>
            <div className="px-4 py-2 bg-gray-100 rounded border border-gray-300 font-mono">
              {originalCard?.accountId}
            </div>
            <p className="text-xs text-gray-500 mt-1">Account ID cannot be changed</p>
          </div>

          {/* Embossed Name */}
          <div>
            <Input
              label="Embossed Name"
              value={formData.embossedName || ''}
              onChange={(e) => {
                setFormData({ ...formData, embossedName: e.target.value });
                validateEmbossedName(e.target.value);
              }}
              required
            />
            {validationErrors.embossedName && (
              <p className="text-sm text-red-600 mt-1">
                {validationErrors.embossedName}
              </p>
            )}
            <p className="text-xs text-gray-500 mt-1">
              Only alphabets and spaces allowed
            </p>
          </div>

          {/* Expiration Date */}
          <div>
            <Input
              label="Expiration Date"
              type="date"
              value={formData.expirationDate || ''}
              onChange={(e) => {
                setFormData({ ...formData, expirationDate: e.target.value });
                validateExpirationDate(e.target.value);
              }}
              required
            />
            {validationErrors.expirationDate && (
              <p className="text-sm text-red-600 mt-1">
                {validationErrors.expirationDate}
              </p>
            )}
            <p className="text-xs text-gray-500 mt-1">
              Must be a future date
            </p>
          </div>

          {/* Active Status */}
          <div>
            <Select
              label="Active Status"
              value={formData.activeStatus || 'Y'}
              onChange={(e) => {
                setFormData({ ...formData, activeStatus: e.target.value });
                validateActiveStatus(e.target.value);
              }}
              options={[
                { value: 'Y', label: 'Active (Y)' },
                { value: 'N', label: 'Inactive (N)' },
              ]}
              required
            />
            {validationErrors.activeStatus && (
              <p className="text-sm text-red-600 mt-1">
                {validationErrors.activeStatus}
              </p>
            )}
          </div>
        </div>

        {/* Form Actions */}
        <div className="mt-8 flex gap-2 pt-6 border-t">
          <Button type="submit" disabled={saving || Object.keys(validationErrors).length > 0}>
            {saving ? 'Saving...' : 'Save Changes'}
          </Button>
          <Button type="button" variant="secondary" onClick={handleCancel}>
            Cancel
          </Button>
          <Button type="button" variant="secondary" onClick={handleBackToList}>
            Back to List
          </Button>
        </div>
      </form>

      {/* Help Text */}
      <div className="mt-6 bg-blue-50 p-4 rounded-lg">
        <h3 className="font-semibold text-blue-900 mb-2">Validation Rules</h3>
        <ul className="text-sm text-blue-800 space-y-1">
          <li>• Embossed name can only contain alphabets and spaces</li>
          <li>• Expiration date must be in the future</li>
          <li>• Active status must be Y (Active) or N (Inactive)</li>
        </ul>
      </div>
    </div>
  );
}
