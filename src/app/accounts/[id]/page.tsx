'use client';

import React, { useEffect, useState, useCallback } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { customerService } from '@/services/customerService';
import { Account, Customer, UpdateAccountRequest, UpdateCustomerRequest } from '@/types/account';
import { Input, Button } from '@/components/ui';

export default function AccountDetailPage() {
  const params = useParams();
  const router = useRouter();
  const [account, setAccount] = useState<Account | null>(null);
  const [customer, setCustomer] = useState<Customer | null>(null);
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [successMessage, setSuccessMessage] = useState<string | null>(null);
  const [isEditing, setIsEditing] = useState(false);
  const [hasChanges, setHasChanges] = useState(false);

  const [accountFormData, setAccountFormData] = useState<UpdateAccountRequest>({});
  const [customerFormData, setCustomerFormData] = useState<UpdateCustomerRequest>({});

  const [originalAccountData, setOriginalAccountData] = useState<Account | null>(null);
  const [originalCustomerData, setOriginalCustomerData] = useState<Customer | null>(null);

  const fetchData = useCallback(async () => {
    if (!params.id) return;

    try {
      setLoading(true);
      setError(null);
      const accountId = parseInt(params.id as string, 10);
      
      const accountData = await accountService.getAccountById(accountId);
      setAccount(accountData);
      setOriginalAccountData(accountData);
      setAccountFormData({
        account_id: accountData.account_id,
        active_status: accountData.active_status,
        current_balance: accountData.current_balance,
        credit_limit: accountData.credit_limit,
        cash_credit_limit: accountData.cash_credit_limit,
        open_date: accountData.open_date,
        expiration_date: accountData.expiration_date,
        reissue_date: accountData.reissue_date,
        current_cycle_credit: accountData.current_cycle_credit,
        current_cycle_debit: accountData.current_cycle_debit,
        group_id: accountData.group_id,
      });

      try {
        const customers = await customerService.getCustomers();
        if (customers && customers.length > 0) {
          const customerData = customers[0];
          setCustomer(customerData);
          setOriginalCustomerData(customerData);
          setCustomerFormData({
            customer_id: customerData.customer_id,
            ssn: customerData.ssn,
            first_name: customerData.first_name,
            middle_name: customerData.middle_name,
            last_name: customerData.last_name,
            address_line_1: customerData.address_line_1,
            address_line_2: customerData.address_line_2,
            address_line_3: customerData.address_line_3,
            city: customerData.city,
            state_code: customerData.state_code,
            country_code: customerData.country_code,
            zip_code: customerData.zip_code,
            phone_number_1: customerData.phone_number_1,
            phone_number_2: customerData.phone_number_2,
            date_of_birth: customerData.date_of_birth,
            government_issued_id: customerData.government_issued_id,
            government_id: customerData.government_id,
            eft_account_id: customerData.eft_account_id,
            primary_holder_indicator: customerData.primary_holder_indicator,
            primary_card_holder_indicator: customerData.primary_card_holder_indicator,
            fico_score: customerData.fico_score,
          });
        }
      } catch (customerErr) {
        console.warn('Customer data not available:', customerErr);
      }
    } catch (err) {
      setError('Failed to load account details');
      console.error('Error fetching account:', err);
    } finally {
      setLoading(false);
    }
  }, [params.id]);

  useEffect(() => {
    fetchData();
  }, [fetchData]);

  useEffect(() => {
    if (!originalAccountData || !originalCustomerData) {
      setHasChanges(false);
      return;
    }

    const accountChanged = JSON.stringify(accountFormData) !== JSON.stringify({
      account_id: originalAccountData.account_id,
      active_status: originalAccountData.active_status,
      current_balance: originalAccountData.current_balance,
      credit_limit: originalAccountData.credit_limit,
      cash_credit_limit: originalAccountData.cash_credit_limit,
      open_date: originalAccountData.open_date,
      expiration_date: originalAccountData.expiration_date,
      reissue_date: originalAccountData.reissue_date,
      current_cycle_credit: originalAccountData.current_cycle_credit,
      current_cycle_debit: originalAccountData.current_cycle_debit,
      group_id: originalAccountData.group_id,
    });

    const customerChanged = JSON.stringify(customerFormData) !== JSON.stringify({
      customer_id: originalCustomerData.customer_id,
      ssn: originalCustomerData.ssn,
      first_name: originalCustomerData.first_name,
      middle_name: originalCustomerData.middle_name,
      last_name: originalCustomerData.last_name,
      address_line_1: originalCustomerData.address_line_1,
      address_line_2: originalCustomerData.address_line_2,
      address_line_3: originalCustomerData.address_line_3,
      city: originalCustomerData.city,
      state_code: originalCustomerData.state_code,
      country_code: originalCustomerData.country_code,
      zip_code: originalCustomerData.zip_code,
      phone_number_1: originalCustomerData.phone_number_1,
      phone_number_2: originalCustomerData.phone_number_2,
      date_of_birth: originalCustomerData.date_of_birth,
      government_issued_id: originalCustomerData.government_issued_id,
      government_id: originalCustomerData.government_id,
      eft_account_id: originalCustomerData.eft_account_id,
      primary_holder_indicator: originalCustomerData.primary_holder_indicator,
      primary_card_holder_indicator: originalCustomerData.primary_card_holder_indicator,
      fico_score: originalCustomerData.fico_score,
    });

    setHasChanges(accountChanged || customerChanged);
  }, [accountFormData, customerFormData, originalAccountData, originalCustomerData]);

  const validateForm = (): boolean => {
    if (!accountFormData.account_id) {
      setError('Account ID is required');
      return false;
    }

    if (!accountFormData.active_status) {
      setError('Active status is required');
      return false;
    }

    if (accountFormData.current_balance === undefined || accountFormData.current_balance === null) {
      setError('Current balance is required');
      return false;
    }

    if (accountFormData.credit_limit === undefined || accountFormData.credit_limit === null) {
      setError('Credit limit is required');
      return false;
    }

    if (!customerFormData.customer_id) {
      setError('Customer ID is required');
      return false;
    }

    if (!customerFormData.ssn) {
      setError('SSN is required');
      return false;
    }

    if (!customerFormData.first_name) {
      setError('First name is required');
      return false;
    }

    if (!customerFormData.last_name) {
      setError('Last name is required');
      return false;
    }

    if (customerFormData.fico_score !== undefined && customerFormData.fico_score !== null) {
      if (customerFormData.fico_score < 300 || customerFormData.fico_score > 850) {
        setError('FICO score must be between 300 and 850');
        return false;
      }
    }

    return true;
  };

  const handleSave = async () => {
    setError(null);
    setSuccessMessage(null);

    if (!hasChanges) {
      setSuccessMessage('No changes were made');
      return;
    }

    if (!validateForm()) {
      return;
    }

    try {
      setSaving(true);
      const accountId = parseInt(params.id as string, 10);

      await accountService.updateAccount(accountId, accountFormData);

      if (customer && customerFormData.customer_id) {
        await customerService.updateCustomer(customerFormData.customer_id, customerFormData);
      }

      setSuccessMessage('Account and customer information updated successfully');
      setIsEditing(false);
      await fetchData();
    } catch (err) {
      setError('Failed to save changes. Please try again.');
      console.error('Error saving data:', err);
    } finally {
      setSaving(false);
    }
  };

  const handleCancel = () => {
    if (originalAccountData) {
      setAccountFormData({
        account_id: originalAccountData.account_id,
        active_status: originalAccountData.active_status,
        current_balance: originalAccountData.current_balance,
        credit_limit: originalAccountData.credit_limit,
        cash_credit_limit: originalAccountData.cash_credit_limit,
        open_date: originalAccountData.open_date,
        expiration_date: originalAccountData.expiration_date,
        reissue_date: originalAccountData.reissue_date,
        current_cycle_credit: originalAccountData.current_cycle_credit,
        current_cycle_debit: originalAccountData.current_cycle_debit,
        group_id: originalAccountData.group_id,
      });
    }

    if (originalCustomerData) {
      setCustomerFormData({
        customer_id: originalCustomerData.customer_id,
        ssn: originalCustomerData.ssn,
        first_name: originalCustomerData.first_name,
        middle_name: originalCustomerData.middle_name,
        last_name: originalCustomerData.last_name,
        address_line_1: originalCustomerData.address_line_1,
        address_line_2: originalCustomerData.address_line_2,
        address_line_3: originalCustomerData.address_line_3,
        city: originalCustomerData.city,
        state_code: originalCustomerData.state_code,
        country_code: originalCustomerData.country_code,
        zip_code: originalCustomerData.zip_code,
        phone_number_1: originalCustomerData.phone_number_1,
        phone_number_2: originalCustomerData.phone_number_2,
        date_of_birth: originalCustomerData.date_of_birth,
        government_issued_id: originalCustomerData.government_issued_id,
        government_id: originalCustomerData.government_id,
        eft_account_id: originalCustomerData.eft_account_id,
        primary_holder_indicator: originalCustomerData.primary_holder_indicator,
        primary_card_holder_indicator: originalCustomerData.primary_card_holder_indicator,
        fico_score: originalCustomerData.fico_score,
      });
    }

    setIsEditing(false);
    setError(null);
    setSuccessMessage(null);
  };

  const handleExit = () => {
    router.push('/accounts/search');
  };

  if (loading) {
    return (
      <div className="min-h-screen bg-gray-50 p-6">
        <div className="max-w-6xl mx-auto">
          <div className="bg-white shadow rounded-lg p-6">
            <p className="text-gray-600">Loading account details...</p>
          </div>
        </div>
      </div>
    );
  }

  if (!account) {
    return (
      <div className="min-h-screen bg-gray-50 p-6">
        <div className="max-w-6xl mx-auto">
          <div className="bg-white shadow rounded-lg p-6">
            <p className="text-red-600">Account not found</p>
            <Button onClick={handleExit} className="mt-4">
              Back to Search
            </Button>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-50 p-6">
      <div className="max-w-6xl mx-auto">
        <div className="bg-white shadow rounded-lg p-6">
          <div className="flex justify-between items-center mb-6">
            <div>
              <h1 className="text-2xl font-bold text-gray-900">Account Details</h1>
              <p className="text-sm text-gray-600 mt-1">
                Account ID: {account.account_id}
              </p>
            </div>
            <div className="flex gap-2">
              {!isEditing ? (
                <>
                  <Button onClick={() => setIsEditing(true)}>
                    Edit
                  </Button>
                  <Button variant="secondary" onClick={handleExit}>
                    Exit
                  </Button>
                </>
              ) : (
                <>
                  <Button
                    onClick={handleSave}
                    disabled={saving || !hasChanges}
                  >
                    {saving ? 'Saving...' : 'Save'}
                  </Button>
                  <Button
                    variant="secondary"
                    onClick={handleCancel}
                    disabled={saving}
                  >
                    Cancel
                  </Button>
                </>
              )}
            </div>
          </div>

          {error && (
            <div className="mb-4 p-4 bg-red-50 border border-red-200 rounded-md">
              <p className="text-sm text-red-800">{error}</p>
            </div>
          )}

          {successMessage && (
            <div className="mb-4 p-4 bg-green-50 border border-green-200 rounded-md">
              <p className="text-sm text-green-800">{successMessage}</p>
            </div>
          )}

          <div className="space-y-8">
            <div>
              <h2 className="text-lg font-semibold text-gray-900 mb-4 pb-2 border-b">
                Account Information
              </h2>
              <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
                <Input
                  label="Account ID"
                  type="number"
                  value={accountFormData.account_id || ''}
                  onChange={(e) => setAccountFormData({ ...accountFormData, account_id: parseInt(e.target.value, 10) })}
                  disabled={!isEditing}
                  required
                />
                <Input
                  label="Active Status"
                  type="text"
                  value={accountFormData.active_status || ''}
                  onChange={(e) => setAccountFormData({ ...accountFormData, active_status: e.target.value })}
                  disabled={!isEditing}
                  required
                />
                <Input
                  label="Current Balance"
                  type="number"
                  step="0.01"
                  value={accountFormData.current_balance || ''}
                  onChange={(e) => setAccountFormData({ ...accountFormData, current_balance: parseFloat(e.target.value) })}
                  disabled={!isEditing}
                  required
                />
                <Input
                  label="Credit Limit"
                  type="number"
                  step="0.01"
                  value={accountFormData.credit_limit || ''}
                  onChange={(e) => setAccountFormData({ ...accountFormData, credit_limit: parseFloat(e.target.value) })}
                  disabled={!isEditing}
                  required
                />
                <Input
                  label="Cash Credit Limit"
                  type="number"
                  step="0.01"
                  value={accountFormData.cash_credit_limit || ''}
                  onChange={(e) => setAccountFormData({ ...accountFormData, cash_credit_limit: parseFloat(e.target.value) })}
                  disabled={!isEditing}
                  required
                />
                <Input
                  label="Open Date"
                  type="date"
                  value={accountFormData.open_date || ''}
                  onChange={(e) => setAccountFormData({ ...accountFormData, open_date: e.target.value })}
                  disabled={!isEditing}
                  required
                />
                <Input
                  label="Expiration Date"
                  type="date"
                  value={accountFormData.expiration_date || ''}
                  onChange={(e) => setAccountFormData({ ...accountFormData, expiration_date: e.target.value })}
                  disabled={!isEditing}
                  required
                />
                <Input
                  label="Reissue Date"
                  type="date"
                  value={accountFormData.reissue_date || ''}
                  onChange={(e) => setAccountFormData({ ...accountFormData, reissue_date: e.target.value })}
                  disabled={!isEditing}
                  required
                />
                <Input
                  label="Current Cycle Credit"
                  type="number"
                  step="0.01"
                  value={accountFormData.current_cycle_credit || ''}
                  onChange={(e) => setAccountFormData({ ...accountFormData, current_cycle_credit: parseFloat(e.target.value) })}
                  disabled={!isEditing}
                  required
                />
                <Input
                  label="Current Cycle Debit"
                  type="number"
                  step="0.01"
                  value={accountFormData.current_cycle_debit || ''}
                  onChange={(e) => setAccountFormData({ ...accountFormData, current_cycle_debit: parseFloat(e.target.value) })}
                  disabled={!isEditing}
                  required
                />
                <Input
                  label="Group ID"
                  type="text"
                  value={accountFormData.group_id || ''}
                  onChange={(e) => setAccountFormData({ ...accountFormData, group_id: e.target.value })}
                  disabled={!isEditing}
                />
              </div>
            </div>

            {customer && (
              <div>
                <h2 className="text-lg font-semibold text-gray-900 mb-4 pb-2 border-b">
                  Customer Information
                </h2>
                <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
                  <Input
                    label="Customer ID"
                    type="number"
                    value={customerFormData.customer_id || ''}
                    onChange={(e) => setCustomerFormData({ ...customerFormData, customer_id: parseInt(e.target.value, 10) })}
                    disabled={!isEditing}
                    required
                  />
                  <Input
                    label="SSN"
                    type="text"
                    value={customerFormData.ssn || ''}
                    onChange={(e) => setCustomerFormData({ ...customerFormData, ssn: e.target.value })}
                    disabled={!isEditing}
                    required
                  />
                  <Input
                    label="First Name"
                    type="text"
                    value={customerFormData.first_name || ''}
                    onChange={(e) => setCustomerFormData({ ...customerFormData, first_name: e.target.value })}
                    disabled={!isEditing}
                    required
                  />
                  <Input
                    label="Middle Name"
                    type="text"
                    value={customerFormData.middle_name || ''}
                    onChange={(e) => setCustomerFormData({ ...customerFormData, middle_name: e.target.value })}
                    disabled={!isEditing}
                  />
                  <Input
                    label="Last Name"
                    type="text"
                    value={customerFormData.last_name || ''}
                    onChange={(e) => setCustomerFormData({ ...customerFormData, last_name: e.target.value })}
                    disabled={!isEditing}
                    required
                  />
                  <Input
                    label="Address Line 1"
                    type="text"
                    value={customerFormData.address_line_1 || ''}
                    onChange={(e) => setCustomerFormData({ ...customerFormData, address_line_1: e.target.value })}
                    disabled={!isEditing}
                    required
                  />
                  <Input
                    label="Address Line 2"
                    type="text"
                    value={customerFormData.address_line_2 || ''}
                    onChange={(e) => setCustomerFormData({ ...customerFormData, address_line_2: e.target.value })}
                    disabled={!isEditing}
                  />
                  <Input
                    label="Address Line 3"
                    type="text"
                    value={customerFormData.address_line_3 || ''}
                    onChange={(e) => setCustomerFormData({ ...customerFormData, address_line_3: e.target.value })}
                    disabled={!isEditing}
                    required
                  />
                  <Input
                    label="City"
                    type="text"
                    value={customerFormData.city || ''}
                    onChange={(e) => setCustomerFormData({ ...customerFormData, city: e.target.value })}
                    disabled={!isEditing}
                  />
                  <Input
                    label="State Code"
                    type="text"
                    value={customerFormData.state_code || ''}
                    onChange={(e) => setCustomerFormData({ ...customerFormData, state_code: e.target.value })}
                    disabled={!isEditing}
                    required
                  />
                  <Input
                    label="Country Code"
                    type="text"
                    value={customerFormData.country_code || ''}
                    onChange={(e) => setCustomerFormData({ ...customerFormData, country_code: e.target.value })}
                    disabled={!isEditing}
                    required
                  />
                  <Input
                    label="ZIP Code"
                    type="text"
                    value={customerFormData.zip_code || ''}
                    onChange={(e) => setCustomerFormData({ ...customerFormData, zip_code: e.target.value })}
                    disabled={!isEditing}
                    required
                  />
                  <Input
                    label="Phone Number 1"
                    type="text"
                    value={customerFormData.phone_number_1 || ''}
                    onChange={(e) => setCustomerFormData({ ...customerFormData, phone_number_1: e.target.value })}
                    disabled={!isEditing}
                    required
                  />
                  <Input
                    label="Phone Number 2"
                    type="text"
                    value={customerFormData.phone_number_2 || ''}
                    onChange={(e) => setCustomerFormData({ ...customerFormData, phone_number_2: e.target.value })}
                    disabled={!isEditing}
                  />
                  <Input
                    label="Date of Birth"
                    type="date"
                    value={customerFormData.date_of_birth || ''}
                    onChange={(e) => setCustomerFormData({ ...customerFormData, date_of_birth: e.target.value })}
                    disabled={!isEditing}
                    required
                  />
                  <Input
                    label="Government Issued ID"
                    type="text"
                    value={customerFormData.government_issued_id || ''}
                    onChange={(e) => setCustomerFormData({ ...customerFormData, government_issued_id: e.target.value })}
                    disabled={!isEditing}
                  />
                  <Input
                    label="EFT Account ID"
                    type="text"
                    value={customerFormData.eft_account_id || ''}
                    onChange={(e) => setCustomerFormData({ ...customerFormData, eft_account_id: e.target.value })}
                    disabled={!isEditing}
                  />
                  <Input
                    label="Primary Holder Indicator"
                    type="text"
                    value={customerFormData.primary_holder_indicator || ''}
                    onChange={(e) => setCustomerFormData({ ...customerFormData, primary_holder_indicator: e.target.value })}
                    disabled={!isEditing}
                    required
                  />
                  <Input
                    label="FICO Score"
                    type="number"
                    value={customerFormData.fico_score || ''}
                    onChange={(e) => setCustomerFormData({ ...customerFormData, fico_score: parseInt(e.target.value, 10) })}
                    disabled={!isEditing}
                    required
                  />
                </div>
              </div>
            )}
          </div>
        </div>
      </div>
    </div>
  );
}
