'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { cardService } from '@/services/cardService';
import { Card, CardUpdateRequest } from '@/types/card';
import { Input, Select, Button } from '@/components/ui';

/**
 * Card Update Screen (COCRDUPC)
 * Allows updating of credit card information
 * 
 * Business Rules:
 * - Embossed name can only contain alphabets and spaces
 * - Active status must be Y or N
 * - Expiration date must be in the future
 * - Card number and account ID cannot be changed
 * - All validations must pass before submission
 */
export default function EditCardPage() {
  const params = useParams();
  const router = useRouter();
  
  const [card, setCard] = useState<Card | null>(null);
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [error, setError] = useState<string | null>(null);
  
  // Form data
  const [embossedName, setEmbossedName] = useState('');
  const [expirationDate, setExpirationDate] = useState('');
  const [activeStatus, setActiveStatus] = useState('Y');
  
  // Validation errors
  const [embossedNameError, setEmbossedNameError] = useState('');
  const [expirationDateError, setExpirationDateError] = useState('');

  useEffect(() => {
    if (params.cardNumber) {
      fetchCard(params.cardNumber as string);
    }
  }, [params.cardNumber]);

  /**
   * Fetch card details and populate form
   */
  const fetchCard = async (cardNumber: string) => {
    try {
      setLoading(true);
      setError(null);
      const data = await cardService.getCardByNumber(cardNumber);
      setCard(data);
      
      // Populate form fields
      setEmbossedName(data.embossedName);
      setExpirationDate(data.expirationDate.split('T')[0]); // Format for date input
      setActiveStatus(data.activeStatus);
    } catch (err) {
      setError('Failed to load card details');
      console.error('Failed to load card:', err);
    } finally {
      setLoading(false);
    }
  };

  /**
   * Validate embossed name
   * Must contain only alphabets and spaces
   */
  const validateEmbossedName = (value: string): boolean => {
    if (!value.trim()) {
      setEmbossedNameError('Embossed name is required');
      return false;
    }
    
    if (!/^[A-Za-z\s]+$/.test(value)) {
      setEmbossedNameError('Embossed name can only contain alphabets and spaces');
      return false;
    }
    
    setEmbossedNameError('');
    return true;
  };

  /**
   * Validate expiration date
   * Must be in the future
   */
  const validateExpirationDate = (value: string): boolean => {
    if (!value) {
      setExpirationDateError('Expiration date is required');
      return false;
    }
    
    const expDate = new Date(value);
    const today = new Date();
    today.setHours(0, 0, 0, 0);
    
    if (expDate <= today) {
      setExpirationDateError('Expiration date must be in the future');
      return false;
    }
    
    setExpirationDateError('');
    return true;
  };

  /**
   * Handle embossed name change
   */
  const handleEmbossedNameChange = (value: string) => {
    setEmbossedName(value);
    validateEmbossedName(value);
  };

  /**
   * Handle expiration date change
   */
  const handleExpirationDateChange = (value: string) => {
    setExpirationDate(value);
    validateExpirationDate(value);
  };

  /**
   * Handle form submission
   */
  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    // Validate all fields
    const isEmbossedNameValid = validateEmbossedName(embossedName);
    const isExpirationDateValid = validateExpirationDate(expirationDate);
    
    if (!isEmbossedNameValid || !isExpirationDateValid) {
      setError('Please correct the validation errors before submitting');
      return;
    }
    
    if (!card) return;
    
    try {
      setSaving(true);
      setError(null);
      
      const updateData: CardUpdateRequest = {
        embossedName: embossedName.trim(),
        expirationDate: expirationDate,
        activeStatus: activeStatus,
      };
      
      await cardService.updateCard(card.cardNumber, updateData);
      
      // Navigate back to detail view
      router.push(`/cards/${card.cardNumber}`);
    } catch (err) {
      setError('Failed to update card');
      console.error('Failed to update card:', err);
    } finally {
      setSaving(false);
    }
  };

  /**
   * Handle cancel
   */
  const handleCancel = () => {
    if (card) {
      router.push(`/cards/${card.cardNumber}`);
    } else {
      router.push('/cards');
    }
  };

  if (loading) {
    return <div className="p-6">Loading card details...</div>;
  }

  if (error && !card) {
    return (
      <div className="p-6">
        <div className="bg-red-50 border border-red-300 text-red-800 px-4 py-3 rounded mb-4">
          {error}
        </div>
        <Button onClick={() => router.push('/cards')} variant="secondary">
          Back to Card List
        </Button>
      </div>
    );
  }

  if (!card) {
    return (
      <div className="p-6">
        <div className="bg-red-50 border border-red-300 text-red-800 px-4 py-3 rounded mb-4">
          Card not found
        </div>
        <Button onClick={() => router.push('/cards')} variant="secondary">
          Back to Card List
        </Button>
      </div>
    );
  }

  return (
    <div className="p-6 max-w-3xl mx-auto">
      {/* Header */}
      <div className="mb-6">
        <h1 className="text-2xl font-bold text-gray-900">Edit Card</h1>
        <p className="text-sm text-gray-600 mt-1">COCRDUPC - Card Update Program</p>
      </div>

      {/* Error Message */}
      {error && (
        <div className="mb-4 bg-red-50 border border-red-300 text-red-800 px-4 py-3 rounded">
          {error}
        </div>
      )}

      {/* Form */}
      <form onSubmit={handleSubmit} className="bg-white shadow rounded-lg p-6">
        {/* Read-only fields */}
        <div className="mb-6 pb-6 border-b border-gray-200">
          <h3 className="text-lg font-semibold text-gray-900 mb-4">Card Identification</h3>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Card Number
              </label>
              <p className="text-lg font-mono text-gray-900 bg-gray-100 px-3 py-2 rounded border border-gray-300">
                {card.cardNumber}
              </p>
              <p className="text-xs text-gray-500 mt-1">Cannot be changed</p>
            </div>
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">
                Account ID
              </label>
              <p className="text-lg font-mono text-gray-900 bg-gray-100 px-3 py-2 rounded border border-gray-300">
                {card.accountId}
              </p>
              <p className="text-xs text-gray-500 mt-1">Cannot be changed</p>
            </div>
          </div>
        </div>

        {/* Editable fields */}
        <div className="space-y-4">
          <h3 className="text-lg font-semibold text-gray-900 mb-4">Card Information</h3>
          
          <Input
            label="Embossed Name"
            value={embossedName}
            onChange={(e) => handleEmbossedNameChange(e.target.value)}
            error={embossedNameError}
            required
            placeholder="Enter name as it appears on card"
            helperText="Only alphabets and spaces allowed"
          />
          
          <Input
            label="Expiration Date"
            type="date"
            value={expirationDate}
            onChange={(e) => handleExpirationDateChange(e.target.value)}
            error={expirationDateError}
            required
            helperText="Must be a future date"
          />
          
          <Select
            label="Active Status"
            value={activeStatus}
            onChange={(e) => setActiveStatus(e.target.value)}
            options={[
              { value: 'Y', label: 'Active (Y)' },
              { value: 'N', label: 'Inactive (N)' },
            ]}
            required
            helperText="Set to 'N' to deactivate the card"
          />
        </div>

        {/* Action Buttons */}
        <div className="mt-6 pt-6 border-t border-gray-200 flex gap-3">
          <Button type="submit" disabled={saving}>
            {saving ? 'Saving Changes...' : 'Save Changes'}
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
      </form>

      {/* Validation Rules */}
      <div className="mt-6 p-4 bg-gray-50 rounded-lg border border-gray-200">
        <h3 className="text-sm font-semibold text-gray-900 mb-2">Validation Rules:</h3>
        <ul className="text-sm text-gray-700 space-y-1">
          <li>• <strong>Embossed Name:</strong> Required, only alphabets and spaces allowed</li>
          <li>• <strong>Expiration Date:</strong> Required, must be in the future</li>
          <li>• <strong>Active Status:</strong> Must be Y (Active) or N (Inactive)</li>
          <li>• <strong>Card Number and Account ID:</strong> Cannot be modified</li>
        </ul>
      </div>

      {/* Instructions */}
      <div className="mt-4 p-4 bg-blue-50 rounded-lg border border-blue-200">
        <h3 className="text-sm font-semibold text-blue-900 mb-2">Instructions:</h3>
        <ul className="text-sm text-blue-800 space-y-1">
          <li>• Make your changes to the editable fields</li>
          <li>• Click <strong>Save Changes</strong> to update the card</li>
          <li>• Click <strong>Cancel</strong> to discard changes and return to detail view</li>
          <li>• All validation rules must be satisfied before saving</li>
        </ul>
      </div>
    </div>
  );
}
